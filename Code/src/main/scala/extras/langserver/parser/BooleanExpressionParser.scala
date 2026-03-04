package extras.langserver.parser

import extras.langserver.parser.ExpressionParser.expr
import extras.langserver.parser.lexer.*
import extras.langserver.parser.lexer.implicits.implicitSymbol
import modularized.stateful.GarbageCollected.*
import parsley.Parsley
import parsley.Parsley.atomic

object BooleanExpressionParser extends PositionedExpressionParser[BooleanExpr] {
  lazy val booleanExpr: Parsley[PositionedExpr] =
    booleanValueExpr <|> unaryBooleanOperation <|> binaryBooleanOperation <|> ifExpr

  private def booleanValue: Parsley[Boolean] = "true" *> Parsley.pure(true) <|> "false" *> Parsley.pure(false)

  private lazy val booleanValueExpr: Parsley[PositionedExpr] = positioned {
    for {
      b <- booleanValue
    } yield if b then True else False
  }

  private lazy val unaryBooleanOperation: Parsley[PositionedExpr] = positioned {
    for {
      op   <- "!" *> Parsley.pure(Not.apply)
      expr <- parentheses(booleanExpr) <|> atomic(booleanValueExpr)
    } yield op(expr)
  }

  private lazy val binaryBooleanOperation: Parsley[PositionedExpr] = positioned {
    for {
      op  <- "&" *> Parsley.pure(And.apply) <|> "|" *> Parsley.pure(Or.apply) <|> "==" *> Parsley.pure(Eq.apply)
      lhs <- parentheses(booleanExpr) <|> atomic(booleanValueExpr)
      rhs <- parentheses(booleanExpr) <|> atomic(booleanValueExpr)
    } yield op(lhs, rhs)
  }

  private lazy val ifExpr: Parsley[PositionedExpr] = positioned {
    for {
      _        <- "if"
      test     <- atomic(booleanExpr) <|> parentheses(atomic(booleanExpr)) // what is with identifiers?
      _        <- "then"
      thenBody <- curlyBrackets(expr)
      _        <- "else"
      elseBody <- curlyBrackets(expr)
    } yield If(test, thenBody, elseBody)
  }

  def parser: Parsley[PositionedExpr] = fully(booleanExpr)
}
