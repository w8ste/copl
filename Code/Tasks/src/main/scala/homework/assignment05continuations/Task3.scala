package homework.assignment05continuations

object Task3 {
  sealed trait FLBE
  case class Bool(v: Boolean)                            extends FLBE
  case class Not(expr: FLBE)                             extends FLBE
  case class And(lhs: FLBE, rhs: FLBE)                   extends FLBE
  case class Or(lhs: FLBE, rhs: FLBE)                    extends FLBE
  case class If(testE: FLBE, thenE: FLBE, elseE: FLBE)   extends FLBE
  case class Let(name: String, namedE: FLBE, body: FLBE) extends FLBE
  case class Id(name: String)                            extends FLBE
  case class Fun(param: String, body: FLBE)              extends FLBE
  case class App(funExpr: FLBE, arg: FLBE)               extends FLBE
  case class BindCC(name: String, body: FLBE)            extends FLBE
  case class Boundary(body: FLBE)                        extends FLBE
  case class Break(expr: FLBE)                           extends FLBE

  case class Closure(param: String, body: FLBE, env: Env)
  case class Continuation(c: Value => Nothing)

  type Value = Bool | Closure | Continuation

  val wellKnowBoundaryName = "boundary continuation"

  type Env = Map[String, Value]

  def interp(expr: FLBE, env: Env, k: Value => Nothing): Nothing = ???

}
