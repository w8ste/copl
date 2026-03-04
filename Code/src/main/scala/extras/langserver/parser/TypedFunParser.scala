package extras.langserver.parser

import extras.langserver.parser.ExpressionParser.expr
import extras.langserver.parser.lexer.*
import extras.langserver.parser.lexer.implicits.implicitSymbol
import modularized.stateful.GarbageCollected.*
import parsley.Parsley
import parsley.Parsley.atomic

object TypedFunParser extends PositionedExpressionParser[TypedFun] {

  private def getTypeFromString(typeStr: String): Type = typeStr match {
    // TODO: Add the rest of the types?
    case "TNum"  => TNum()
    case "TBool" => TBool
    case "TFun"  => TFun(TUnspecified, TUnspecified) // does this make sense or do we need to specify the function type?
    case "TRecord"      => TRecord(Map.empty) // do we need to specify the record type?
    case "TUnspecified" => TUnspecified
    case "Error"        => Error("")
    case _              => throw new IllegalArgumentException(s"Unknown type: $typeStr")
  }

  /* TODO: Add syntax for that?*/
  lazy val typedFunExpr: Parsley[PositionedExpr] = positioned {
    for {
      _         <- "fun"
      _         <- "("
      param     <- identifier
      paramType <- ":" *> identifier
      _         <- ")"
      body      <- curlyBrackets(atomic(expr))
    } yield TypedFun(param, getTypeFromString(paramType), body)
  }

  def parser: Parsley[PositionedExpr] = fully(typedFunExpr)
}
