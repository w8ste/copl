package extras.langserver.parser

import extras.langserver.parser.ExpressionParser.expr
import extras.langserver.parser.lexer.*
import extras.langserver.parser.lexer.implicits.implicitSymbol
import modularized.stateful.GarbageCollected.*
import parsley.Parsley
import parsley.Parsley.atomic

object BoxExpressionParser extends PositionedExpressionParser[BoxExpr] {
  lazy val boxExpr: Parsley[PositionedExpr] = newBoxExpr <|> setBoxExpr <|> openBoxExpr

  private lazy val newBoxExpr: Parsley[PositionedExpr] = positioned {
    for {
      valExpr <- "box-new" *> curlyBrackets(atomic(expr))
    } yield NewBox(valExpr)
  }

  private lazy val setBoxExpr: Parsley[PositionedExpr] = positioned {
    for {
      boxExpr   <- "box-set" *> curlyBrackets(atomic(boxExpr))
      valueExpr <- "=" *> curlyBrackets(atomic(expr))
    } yield SetBox(boxExpr, valueExpr)
  }

  private lazy val openBoxExpr: Parsley[PositionedExpr] = positioned {
    for {
      boxExpr <- "box-open" *> curlyBrackets(atomic(boxExpr))
    } yield OpenBox(boxExpr)
  }

  def parser: Parsley[PositionedExpr] = fully(boxExpr)
}
