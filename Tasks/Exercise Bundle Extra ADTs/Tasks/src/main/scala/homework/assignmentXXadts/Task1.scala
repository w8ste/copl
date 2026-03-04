package homework.assignmentXXadts

import modularized.environmental.FullInterpreter.*

import scala.language.implicitConversions

object Task1 {
  implicit def symbolToExpr(symbol: String): Id = Id(symbol)
  implicit def intToExpr(n: Int): Num           = Num(n)

  val booleanInterp: LetRec = LetRec(
    "interp",
    TypedFun(
      "expr",
      TUnspecified,
      ???
    ),
    "interp"
  )
}
