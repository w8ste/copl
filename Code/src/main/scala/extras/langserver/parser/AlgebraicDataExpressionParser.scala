package extras.langserver.parser

import extras.langserver.parser.ExpressionParser.expr
import extras.langserver.parser.lexer.*
import extras.langserver.parser.lexer.implicits.implicitSymbol
import modularized.stateful.GarbageCollected.*
import parsley.Parsley
import parsley.combinator.sepBy1

object AlgebraicDataExpressionParser extends PositionedExpressionParser[AlgebraicDataExpr] {
  lazy val algebraicDataExpr: Parsley[PositionedExpr] = data

  private lazy val data: Parsley[PositionedExpr] = positioned {
    for {
      _    <- "data"
      name <- identifier
      args <- curlyBrackets(sepBy1(expr, ","))
    } yield Data(name, args)
  }

  def parser: Parsley[PositionedExpr] = fully(algebraicDataExpr)
}
