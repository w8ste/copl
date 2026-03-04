package extras.langserver.parser

import extras.langserver.parser.ExpressionParser.expr
import extras.langserver.parser.lexer.*
import extras.langserver.parser.lexer.implicits.implicitSymbol
import modularized.stateful.GarbageCollected.*
import parsley.Parsley
import parsley.combinator.sepBy1

object SeqnParser extends PositionedExpressionParser[Seqn] {
  lazy val seqn: Parsley[PositionedExpr] = positioned {
    for {
      _ <-
        "seqn" // This is required to prevent infinite recursion in the expresion parser. The other solution would be to set a sequence as default.
      expressions <- sepBy1(expr, ";")
    } yield Seqn(expressions.map(_.asInstanceOf[Expr])*)
  }

  def parser: Parsley[PositionedExpr] = fully(seqn)
}
