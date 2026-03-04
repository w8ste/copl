package extras.langserver

import extras.langserver.parser.ExpressionParser.*
import extras.langserver.parser.{COPLError, COPLErrorBuilder}
import modularized.stateful.GarbageCollected.*
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.TextDocumentService
import parsley.{Failure, Success}

import java.util
import java.util.Collections
import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.language.{postfixOps, unsafeNulls}

class CoplTextDocumentService(server: CoplLanguageServer) extends TextDocumentService {

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val document = params.getTextDocument
    server.getOpenDocuments(document.getUri) = document.getText

    publishDiagnostics(document.getUri, document.getText)
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val changes = params.getContentChanges
    if changes.isEmpty then return

    val documentUri = params.getTextDocument.getUri
    val newText     = changes.get(0).getText
    server.getOpenDocuments(documentUri) = newText

    publishDiagnostics(documentUri, newText)
  }

  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    val documentUri = params.getTextDocument.getUri
    server.getOpenDocuments.remove(documentUri)

    server.clearDiagnostic(documentUri)
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    val documentUri  = params.getTextDocument.getUri
    val documentText = server.getOpenDocuments.getOrElse(documentUri, "")

    publishDiagnostics(documentUri, documentText)
  }

  override def completion(params: CompletionParams)
      : CompletableFuture[Either[util.List[CompletionItem], CompletionList]] = {
    val documentText = server.getOpenDocuments.getOrElse(params.getTextDocument.getUri, "")

    parser.parse(documentText)(using COPLErrorBuilder) match {
      case Success(_) =>
        CompletableFuture.completedFuture(Either.forLeft(Collections.emptyList[CompletionItem]()))
      case Failure(error: COPLError) =>
        val expectedTokens = extractExpectedTokens(error)
        val completions    = expectedTokens.map { token =>
          val item = new CompletionItem(token)
          item.setKind(CompletionItemKind.Keyword)
          item
        }
        CompletableFuture.completedFuture(Either.forLeft(completions.toIndexedSeq.asJava))
    }
  }

  override def semanticTokensFull(params: SemanticTokensParams): CompletableFuture[SemanticTokens] = {
    val documentUri = params.getTextDocument.getUri
    val text        = server.getOpenDocuments.getOrElse(documentUri, "")
    val tokens      = computeSemanticTokens(text)

    val semanticTokens = new SemanticTokens(tokens.asJava)
    CompletableFuture.completedFuture(semanticTokens)
  }

  override def semanticTokensRange(params: SemanticTokensRangeParams): CompletableFuture[SemanticTokens] = {
    val documentUri = params.getTextDocument.getUri
    val text        = server.getOpenDocuments.getOrElse(documentUri, "")
    val range       = params.getRange
    val rangeText   = text.split("\n").slice(range.getStart.getLine, range.getEnd.getLine + 1).mkString("\n")
    val tokens      = computeSemanticTokens(rangeText, Some(range))

    val semanticTokens = new SemanticTokens(tokens.asJava)
    CompletableFuture.completedFuture(semanticTokens)
  }

  private def publishDiagnostics(documentUri: String, text: String): Unit = {
    var diagnostic = new Diagnostic()
    try {
      parser.parse(text)(using COPLErrorBuilder) match {
        case Success(expr: Expr) =>
          val result = interp(expr)
          diagnostic = createDiagnostic(
            DiagnosticSeverity.Information,
            s"Result of expression: ${result}",
            new Range(new Position(0, 0), new Position(text.split("\n").length, text.split("\n").last.length))
          )
        case Failure(error: COPLError) =>
          val errorPosition = extractErrorPosition(error).getOrElse(new Position(0, 0))
          diagnostic = createDiagnostic(DiagnosticSeverity.Error, s"${unexpectedMessage(error)}", errorPosition)
      }
    } catch {
      case e: RuntimeException =>
        diagnostic = createDiagnostic(DiagnosticSeverity.Error, e.getMessage, new Position(0, 0))
    }

    val diagnostics = List(diagnostic)
    server.publishDiagnostics(documentUri, diagnostics)
  }

  private def createDiagnostic(severity: DiagnosticSeverity, message: String, range: Range): Diagnostic = {
    val diagnostic = new Diagnostic()
    diagnostic.setSeverity(severity)
    diagnostic.setMessage(message)
    diagnostic.setRange(range)
    diagnostic
  }

  private def createDiagnostic(severity: DiagnosticSeverity, message: String, position: Position): Diagnostic = {
    val diagnostic = new Diagnostic()
    diagnostic.setSeverity(severity)
    diagnostic.setMessage(message)
    diagnostic.setRange(new Range(position, position))
    diagnostic
  }

  private def extractErrorPosition(error: COPLError): Option[Position] = {
    error match {
      case COPLError((line, column), _) =>
        Some(new Position(line - 1, column - 1))

    }
  }

  private def extractExpectedTokens(error: COPLError): Seq[String] = {
    error match
        case COPLError(_, expected) => expected
  }

  private def unexpectedMessage(error: COPLError): String = {
    error match
        case COPLError(pos, expected) =>
          val expectedStr = expected.mkString(", ")
          s"Unexpected token at position ${pos._1}:${pos._2}. \n Expected $expectedStr."
  }

  private def computeSemanticTokens(text: String, range: Option[Range] = None): List[Integer] = {
    val tokens = mutable.ListBuffer[(Int, Int, Int, Int, Int)]()
    val expr   = parser.parse(text) match {
      case Success(expr) => expr
      case Failure(_)    => return List()
    }
    extractTokens(expr, tokens)

    // Convert the token list to the required format
    var lastLine      = 0
    var lastStartChar = 0

    tokens.toList.flatMap { case (line, startChar, length, tokenType, tokenModifiers) =>
      val deltaLine      = line - lastLine
      val deltaStartChar = if deltaLine == 0 then startChar - lastStartChar else startChar
      lastLine = line
      lastStartChar = startChar
      List(deltaLine, deltaStartChar, length, tokenType, tokenModifiers)
    }
  }

  private def extractTokens(
      node: Expr,
      tokens: mutable.ListBuffer[(Int, Int, Int, Int, Int)],
      position: ((Int, Int), (Int, Int)) = ((0, 0), (0, 0))
  ): Unit = {
    def handlePosition(expr: Expr, pos: ((Int, Int), (Int, Int))): Unit =
      tokens += ((pos._1._1, pos._1._2, pos._2._2 - pos._1._2, extractTokenType(expr), extractTokenModifiers(expr)))

    // TODO: This has to be updated to match the token types and modifiers of the language but therefor the feature has to work on the client side first
    def extractTokenType(expr: Expr): Int = expr match {
      case _: Num       => 3
      case _: Id        => 1
      case True | False => 4
      case _            => 1
    }

    def extractTokenModifiers(expr: Expr): Int = expr match {
      case _: TypedFun => 1
      case _: LetRec   => 1
      case _           => 1
    }

    node match {
      case extras.langserver.parser.Position(expr, (startLine, startChar), (endLine, endChar)) =>
        extractTokens(expr, tokens, ((startLine, startChar), (endLine, endChar)))

      // Arithmetic expressions
      case Num(_) =>
        handlePosition(node, position)
      case Add(lhs, rhs) =>
        handlePosition(node, position)
        extractTokens(lhs, tokens, position)
        extractTokens(rhs, tokens, position)
      case Sub(lhs, rhs) =>
        handlePosition(node, position)
        extractTokens(lhs, tokens, position)
        extractTokens(rhs, tokens, position)
      case Mult(lhs, rhs) =>
        handlePosition(node, position)
        extractTokens(lhs, tokens, position)
        extractTokens(rhs, tokens, position)
      case If0(test, thenBody, elseBody) =>
        handlePosition(node, position)
        extractTokens(test, tokens, position)
        extractTokens(thenBody, tokens, position)
        extractTokens(elseBody, tokens, position)

      // Boolean expressions
      case Not(expr) =>
        handlePosition(node, position)
        extractTokens(expr, tokens, position)
      case And(lhs, rhs) =>
        handlePosition(node, position)
        extractTokens(lhs, tokens, position)
        extractTokens(rhs, tokens, position)
      case Or(lhs, rhs) =>
        handlePosition(node, position)
        extractTokens(lhs, tokens, position)
        extractTokens(rhs, tokens, position)
      case Eq(lhs, rhs) =>
        handlePosition(node, position)
        extractTokens(lhs, tokens, position)
        extractTokens(rhs, tokens, position)
      case If(test, thenBody, elseBody) =>
        handlePosition(node, position)
        extractTokens(test, tokens, position)
        extractTokens(thenBody, tokens, position)
        extractTokens(elseBody, tokens, position)

      // Lambda calculus
      case Id(_) =>
        handlePosition(node, position)
      case Fun(_, body) =>
        handlePosition(node, position)
        extractTokens(body, tokens, position)
      case App(funExp, arg) =>
        handlePosition(node, position)
        extractTokens(funExp, tokens, position)
        extractTokens(arg, tokens, position)
      /*case AppFirstOrder(_, arg) => This is not handled in the parser as instructed
        extractTokens(arg, tokens, position)*/

      // Extensions to the lambda calculus
      case TypedFun(_, _, body) =>
        handlePosition(node, position)
        extractTokens(body, tokens, position)
      case Let(_, namedExpr, body) =>
        handlePosition(node, position)
        extractTokens(namedExpr, tokens, position)
        extractTokens(body, tokens, position)
      case LetRec(_, namedExpr, body) =>
        handlePosition(node, position)
        extractTokens(namedExpr, tokens, position)
        extractTokens(body, tokens, position)

      // Mutability
      case Seqn(exprs*) =>
        handlePosition(node, position)
        exprs.foreach(expr => extractTokens(expr, tokens, position))

      case SetId(_, expr) =>
        handlePosition(node, position)
        extractTokens(expr, tokens, position)

      case NewBox(expr) =>
        handlePosition(node, position)
        extractTokens(expr, tokens, position)
      case SetBox(box, expr) =>
        handlePosition(node, position)
        extractTokens(box, tokens, position)
        extractTokens(expr, tokens, position)
      case OpenBox(box) =>
        handlePosition(node, position)
        extractTokens(box, tokens, position)

      // Data structures
      case Record(fields) =>
        handlePosition(node, position)
        fields.values.foreach(expr => extractTokens(expr, tokens, position))
      case RecordProjection(record, _) =>
        handlePosition(node, position)
        extractTokens(record, tokens, position)

      case Data(_, args: List[Expr]) =>
        handlePosition(node, position)
        args.foreach(expr => extractTokens(expr, tokens, position))

      case Match(expr, cases: List[(_, _, Expr)]) =>
        handlePosition(node, position)
        extractTokens(expr, tokens, position)
        cases.foreach { case (_, _, expr) => extractTokens(expr, tokens, position) }

      case _ =>
    }
  }

  implicit val customErrorBuilder: COPLErrorBuilder = COPLErrorBuilder
}
