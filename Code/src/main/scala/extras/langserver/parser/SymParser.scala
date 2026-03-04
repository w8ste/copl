package extras.langserver.parser

import extras.langserver.parser.lexer.*
import modularized.stateful.GarbageCollected.*
import parsley.Parsley

object SymParser extends PositionedExpressionParser[Sym] {
  lazy val sym: Parsley[PositionedExpr] = positioned {
    for {
      name <- identifier
    } yield Sym(name)
  }

  def parser: Parsley[PositionedExpr] = fully(sym)
}
