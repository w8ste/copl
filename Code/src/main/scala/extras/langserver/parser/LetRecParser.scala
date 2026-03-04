package extras.langserver.parser

import extras.langserver.parser.ExpressionParser.expr
import extras.langserver.parser.lexer.*
import extras.langserver.parser.lexer.implicits.implicitSymbol
import modularized.stateful.GarbageCollected.*
import parsley.Parsley
import parsley.Parsley.atomic

object LetRecParser extends PositionedExpressionParser[LetRec] {
  /* TODO: Ask how this is different from the non recursive let parser? */
  lazy val letRec: Parsley[PositionedExpr] = positioned {
    for {
      name      <- "let-rec" *> identifier
      namedExpr <- "=" *> atomic(expr)
      body      <- "in" *> curlyBrackets(expr)
    } yield LetRec(name, namedExpr, body)
  }

  def parser: Parsley[PositionedExpr] = fully(letRec)
}
