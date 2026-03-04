package interpreters.basic

object ArithmeticExpressions {

  // Define the expression language
  // sealed = trait can only be extended in the same scope
  sealed trait Expr
  case class Num(n: Int)               extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Sub(lhs: Expr, rhs: Expr) extends Expr

  /* Define the type of values that are produced by the interpreter.
   * Note: this just defines Value to be an alias for Int,
   * but later we will see other interpreters with different value types  */
  type Value = Int

  /* Define the interpreter for the language */
  def interp(expr: Expr): Value = expr match {
    case Num(n)        => n
    case Add(lhs, rhs) => interp(lhs) + interp(rhs)
    case Sub(lhs, rhs) => interp(lhs) - interp(rhs)
  }

}
