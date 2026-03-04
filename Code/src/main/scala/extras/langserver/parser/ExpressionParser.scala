package extras.langserver.parser

import extras.langserver.parser.AlgebraicDataExpressionParser.algebraicDataExpr
import extras.langserver.parser.ArithmeticExpressionParser.arithmeticExpr
import extras.langserver.parser.BooleanExpressionParser.booleanExpr
import extras.langserver.parser.BoxExpressionParser.boxExpr
import extras.langserver.parser.FunctionExpressionParser.funExpr
import extras.langserver.parser.LetParser.let
import extras.langserver.parser.LetRecParser.letRec
import extras.langserver.parser.RecordExpressionParser.recordExpr
import extras.langserver.parser.SeqnParser.seqn
import extras.langserver.parser.SetIdParser.setId
import extras.langserver.parser.SymParser.sym
import extras.langserver.parser.TypedFunParser.typedFunExpr
import extras.langserver.parser.lexer.fully
import modularized.stateful.GarbageCollected.Expr
import parsley.Parsley
import parsley.Parsley.atomic

object ExpressionParser extends Parser[Expr] {
  // This should be all expressions that are subexpressions of an expression (ref. GarbageCollected.Expr)
  lazy val expr: Parsley[Expr] = atomic(arithmeticExpr) <|> atomic(typedFunExpr) <|> atomic(booleanExpr) <|> atomic(
    letRec
  ) <|> atomic(let) <|> atomic(recordExpr) <|> atomic(algebraicDataExpr) <|> atomic(setId) <|> atomic(seqn) <|> atomic(
    boxExpr
  ) <|> atomic(funExpr) <|> atomic(sym)

  def parser: Parsley[Expr] = fully(expr)
}
