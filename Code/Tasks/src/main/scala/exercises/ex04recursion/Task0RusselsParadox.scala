package exercises.ex04recursion

object Task0RusselsParadox {
  sealed trait FE
  case class Id(name: String)             extends FE
  case class Fun(param: String, body: FE) extends FE
  case class App(funExp: FE, arg: FE)     extends FE

  type Value = Fun

  def freeVariables(expr: FE): Set[String] = expr match {
    case Id(x)            => Set(x)
    case Fun(param, body) => freeVariables(body) - param
    case App(e1, e2)      => freeVariables(e1) ++ freeVariables(e2)
  }

  def subst(expr: FE, substId: String, value: FE): FE = expr match {
    case Id(name) =>
      if substId == name then value
      else expr

    case Fun(param, body) =>
      if param == substId then
          Fun(param, body)
      else if !freeVariables(value).contains(param) then
          Fun(param, subst(body, substId, value))
      else
          sys.error("Can only substitute expression if the value does not contain the parameter as free variable")

    case App(funExpr, argExpr) =>
      App(subst(funExpr, substId, value), subst(argExpr, substId, value))
  }

  def interp(expr: FE): Value = expr match {
    case Id(name) => sys.error("found unbound id " + name)

    case f @ Fun(_, _) => f

    case App(funExpr, argExpr) =>
      interp(funExpr) match {
        case Fun(param, body) => interp(subst(body, param, argExpr))
      }
  }

  import scala.language.implicitConversions
  implicit def symbolToFE(symbol: String): Id = Id(symbol)

  // Data definitions
  def T: FE = Fun("x", Fun("y", "x")) // true
  def F: FE = Fun("x", Fun("y", "y")) // false

  def ifte(cond: FE, thenBranch: FE, elseBranch: FE): FE = ???

  def not(e: FE): FE = ???

  def and(e1: FE, e2: FE): FE = ???

  def or(e1: FE, e2: FE): FE =
    ???

  /* Task 0, 1 */
  def union(s1: FE, s2: FE): FE = ???

  def intersect(s1: FE, s2: FE): FE = ???

  /* Task 0, 2 */
  // Russel’s set
  def R: FE = ???

  def main(args: Array[String]): Unit = ???

}
