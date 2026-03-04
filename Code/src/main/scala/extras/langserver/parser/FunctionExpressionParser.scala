package extras.langserver.parser

import extras.langserver.parser.ExpressionParser.expr
import extras.langserver.parser.lexer.*
import extras.langserver.parser.lexer.implicits.implicitSymbol
import modularized.stateful.GarbageCollected.*
import parsley.Parsley
import parsley.Parsley.atomic

import scala.language.{existentials, implicitConversions, postfixOps}

object FunctionExpressionParser extends PositionedExpressionParser[FunExpr] {
  lazy val funExpr: Parsley[PositionedExpr] = identifierExpr <|> function <|> applicationExpr

  private lazy val identifierExpr: Parsley[PositionedExpr] = positioned {
    for {
      id <- identifier
    } yield Id(id)
  }

  private lazy val function: Parsley[PositionedExpr] = positioned {
    for {
      _     <- "def"
      param <- identifier
      body  <- curlyBrackets(atomic(expr))
    } yield Fun(param, body)
  }

  private lazy val applicationExpr: Parsley[PositionedExpr] = positioned {
    for {
      _       <- "app"
      funExpr <- curlyBrackets(atomic(funExpr))
      _       <- "with"
      arg     <- curlyBrackets(atomic(expr))
    } yield App(funExpr, arg)
  }

  def parser: Parsley[PositionedExpr] = fully(funExpr)
}
