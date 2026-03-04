package extras.langserver.parser

import extras.langserver.parser.ExpressionParser.expr
import extras.langserver.parser.lexer.*
import extras.langserver.parser.lexer.implicits.implicitSymbol
import modularized.stateful.GarbageCollected.*
import parsley.Parsley
import parsley.Parsley.atomic

object LetParser extends PositionedExpressionParser[Let] {
  lazy val let: Parsley[PositionedExpr] = positioned {
    for {
      _     <- "let"
      id    <- identifier
      _     <- "="
      value <- atomic(expr)
      _     <- "in"
      /*body <- curlyBrackets(notFollowedBy("let") *> expr)*/
      body <- curlyBrackets(atomic(expr))
    } yield Let(id, value, body)
  }

  def parser: Parsley[PositionedExpr] = fully(let)
}
