package extras.langserver.parser

import extras.langserver.parser.ExpressionParser.expr
import extras.langserver.parser.lexer.*
import extras.langserver.parser.lexer.implicits.implicitSymbol
import modularized.stateful.GarbageCollected.*
import parsley.Parsley
import parsley.Parsley.atomic

object SetIdParser extends PositionedExpressionParser[SetId] {
  lazy val setId: Parsley[PositionedExpr] = positioned {
    for {
      _         <- "set"
      id        <- identifier
      valueExpr <- "=" *> atomic(expr)
    } yield SetId(id, valueExpr)
  }

  def parser: Parsley[PositionedExpr] = fully(setId)
}
