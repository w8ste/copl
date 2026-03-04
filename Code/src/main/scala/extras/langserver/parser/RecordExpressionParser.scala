package extras.langserver.parser

import extras.langserver.parser.ExpressionParser.expr
import extras.langserver.parser.lexer.*
import extras.langserver.parser.lexer.implicits.implicitSymbol
import modularized.stateful.GarbageCollected.*
import parsley.Parsley
import parsley.Parsley.atomic
import parsley.combinator.sepBy1

object RecordExpressionParser extends PositionedExpressionParser[RecordExpr] {
  lazy val recordExpr: Parsley[PositionedExpr] = record <|> recordProjection

  private lazy val record: Parsley[PositionedExpr] = positioned {
    for {
      _      <- "{"
      fields <- sepBy1(field, ",").map(_.toMap)
      _      <- "}"
    } yield Record(fields)
  }

  private lazy val field: Parsley[(String, Expr)] = for {
    key   <- identifier
    _     <- "="
    value <- atomic(expr)
  } yield (key, value)

  /* TODO: find good fitting syntax for that */
  private lazy val recordProjection: Parsley[PositionedExpr] = positioned {
    for {
      _     <- "record"
      rec   <- record
      label <- identifier
    } yield RecordProjection(rec, label)
  }

  def parser: Parsley[PositionedExpr] = fully(recordExpr)
}
