package extras.langserver.parser

import extras.langserver.parser.ExpressionParser.expr
import extras.langserver.parser.FunctionExpressionParser.funExpr
import extras.langserver.parser.lexer.*
import extras.langserver.parser.lexer.implicits.implicitSymbol
import modularized.stateful.GarbageCollected.*
import parsley.Parsley
import parsley.Parsley.atomic

object ArithmeticExpressionParser extends PositionedExpressionParser[ArithmeticExpr] {
  lazy val arithmeticExpr: Parsley[PositionedExpr] = numberExpr <|> sExprArithmetic <|> if0Expr

  private lazy val numberExpr: Parsley[PositionedExpr] = positioned {
    for {
      n <- number
    } yield Num(n)
  }

  private lazy val sExprArithmetic: Parsley[PositionedExpr] = positioned {
    for {
      op  <- "+" *> Parsley.pure(Add.apply) <|> "-" *> Parsley.pure(Sub.apply) <|> "*" *> Parsley.pure(Mult.apply)
      lhs <- parentheses(arithmeticExpr) <|> atomic(numberExpr) <|> funExpr
      rhs <- parentheses(arithmeticExpr) <|> atomic(numberExpr) <|> funExpr
    } yield op(lhs, rhs)
  }

  private lazy val if0Expr: Parsley[PositionedExpr] = positioned {
    for {
      _        <- "if0"
      test     <- parentheses(atomic(arithmeticExpr))
      _        <- "then"
      thenBody <- curlyBrackets(expr)
      _        <- "else"
      elseBody <- curlyBrackets(expr)
    } yield If0(test, thenBody, elseBody)
  }

  def parser: Parsley[PositionedExpr] = fully(arithmeticExpr)
}
