package exercises.ex04recursion

object Task1Fixpoint {

  sealed trait FE
  case class Id(name: String)             extends FE
  case class Fun(param: String, body: FE) extends FE
  case class App(funExp: FE, arg: FE)     extends FE

  // Extension of FE with arithmetic expressions
  case class Num(n: Int)                                   extends FE
  case class Add(lhs: FE, rhs: FE)                         extends FE
  case class Sub(lhs: FE, rhs: FE)                         extends FE
  case class Mult(lhs: FE, rhs: FE)                        extends FE
  case class If0(cond: FE, thenBranch: FE, elseBranch: FE) extends FE

  type Value = Fun | Num

  def freeVariables(expr: FE): Set[String] = expr match {
    case Id(x)            => Set(x)
    case Fun(param, body) => freeVariables(body) - param
    case App(e1, e2)      => freeVariables(e1) ++ freeVariables(e2)

    case Num(_)       => Set.empty
    case Add(e1, e2)  => freeVariables(e1) ++ freeVariables(e2)
    case Sub(e1, e2)  => freeVariables(e1) ++ freeVariables(e2)
    case Mult(e1, e2) => freeVariables(e1) ++ freeVariables(e2)
    case If0(c, t, e) => freeVariables(c) ++ freeVariables(t) ++ freeVariables(e)
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

    case Num(_) => expr

    case Add(lhs, rhs) =>
      Add(subst(lhs, substId, value), subst(rhs, substId, value))

    case Sub(lhs, rhs) =>
      Sub(subst(lhs, substId, value), subst(rhs, substId, value))

    case Mult(lhs, rhs) =>
      Mult(subst(lhs, substId, value), subst(rhs, substId, value))

    case If0(c, t, e) =>
      If0(subst(c, substId, value), subst(t, substId, value), subst(e, substId, value))
  }

  def interp(expr: FE): Value = expr match {
    case Id(name) => sys.error("found unbound id " + name)

    case f @ Fun(_, _) => f

    case App(funExpr, argExpr) => interp(funExpr) match {
        case Fun(param, body) => interp(subst(body, param, argExpr))
        case v1               => sys.error(s"Expected function value but got $v1")
      }

    case n @ Num(_) => n

    case Add(lhs, rhs) => (interp(lhs), interp(rhs)) match {
        case (Num(n1), Num(n2)) => Num(n1 + n2)
        case x                  => sys.error(s"expected num but got: $x")
      }
    case Sub(lhs, rhs) => (interp(lhs), interp(rhs)) match {
        case (Num(n1), Num(n2)) => Num(n1 - n2)
        case x                  => sys.error(s"expected num but got: $x")
      }

    case Mult(lhs, rhs) => (interp(lhs), interp(rhs)) match {
        case (Num(n1), Num(n2)) => Num(n1 * n2)
        case x                  => sys.error(s"expected num but got: $x")
      }

    case If0(c, t, e) => interp(c) match {
        case Num(0) => interp(t)
        case _      => interp(e)
      }
  }

  import scala.language.implicitConversions
  implicit def symbolToFE(symbol: String): Id = Id(symbol)
  implicit def intToFE(i: Int): Num           = Num(i)

  /* Task 2, 1 */
  val omega: App = App(Fun("x", App("x", "x")), Fun("x", App("x", "x")))

  def factorialFLAE = ???

  def main(args: Array[String]): Unit =
    ???

}
