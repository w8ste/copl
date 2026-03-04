package extras.langserver.parser

import modularized.stateful.GarbageCollected.Expr
import parsley.Parsley

trait Parser[Expr] {
  def parser: Parsley[Expr]
}

sealed trait PositionedExpr
case class Position(expr: Expr, start: (Int, Int), end: (Int, Int)) extends PositionedExpr
