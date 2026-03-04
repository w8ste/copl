package extras.langserver.parser

import extras.langserver.parser.lexer.implicits.implicitSymbol
import parsley.Parsley
import parsley.token.descriptions.*
import parsley.token.descriptions.numeric.NumericDesc
import parsley.token.predicate.Basic
import parsley.token.symbol.ImplicitSymbol
import parsley.token.{Lexer, predicate}

import scala.language.implicitConversions

object lexer {

  private val desc: LexicalDesc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(c => Character.isLetter(c)),
      identifierLetter = predicate.Basic(c => Character.isLetter(c)),
    ),
    symbolDesc = SymbolDesc.plain.copy(
      // TODO: add more keywords here if needed
      hardKeywords = Set(
        "data",
        "true",
        "false",
        "if",
        "then",
        "else",
        "def",
        "let",
        "let-rec",
        "in",
        "app",
        "with",
        "if0",
        "seqn",
        "box-new",
        "box-set",
        "box-open",
        "set",
        "record",
        "fun"
      ),
      hardOperators = Set("+", "-", "&", "|", "==", "!"),
    ),
    numericDesc =
      NumericDesc.plain.copy(
      ),
    textDesc = text.TextDesc.plain.copy(
      // escapeSequences = text.EscapeDesc.haskell,
    ),
    SpaceDesc.plain.copy(
      commentLine = "//",
      commentStart = "/*",
      commentEnd = "*/",
      space = Basic(c => Character.isWhitespace(c)),
    )
  )
  private val lexer = new Lexer(desc)

  val identifier: Parsley[String]                 = lexer.lexeme.names.identifier
  val number: Parsley[Int]                        = lexer.lexeme.natural.decimal.map(_.toInt)
  val string: Parsley[String]                     = lexer.lexeme.string.fullUtf16
  def parentheses[A](p: Parsley[A]): Parsley[A]   = "(" *> p <* ")"
  def curlyBrackets[A](p: Parsley[A]): Parsley[A] = "{" *> p <* "}"

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

  val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
}
