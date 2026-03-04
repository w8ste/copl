package extras.langserver.parser

import modularized.stateful.GarbageCollected.Expr
import parsley.{Parsley, position}

trait PositionedExpressionParser[T <: Expr] extends Parser[PositionedExpr] {
  def positioned(parser: Parsley[T]): Parsley[PositionedExpr] = for {
    start <- position.pos
    expr  <- parser
    end   <- position.pos
  } yield Position(expr, start, end)
}
