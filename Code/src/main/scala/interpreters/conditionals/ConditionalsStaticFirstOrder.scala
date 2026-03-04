package interpreters.conditionals

object ConditionalsStaticFirstOrder {
  sealed trait ConditionalsF1LAE
  type CF1LAE = ConditionalsF1LAE
  case class Num(n: Int)                                         extends CF1LAE
  case class Add(lhs: CF1LAE, rhs: CF1LAE)                       extends CF1LAE
  case class Sub(lhs: CF1LAE, rhs: CF1LAE)                       extends CF1LAE
  case class Mult(lhs: CF1LAE, rhs: CF1LAE)                      extends CF1LAE
  case class Let(name: String, namedExpr: CF1LAE, body: CF1LAE)  extends CF1LAE
  case class Id(name: String)                                    extends CF1LAE
  case class If0(test: CF1LAE, posBody: CF1LAE, negBody: CF1LAE) extends CF1LAE
  case class App(funName: String, arg: CF1LAE)                   extends CF1LAE

  case class FunDef(argName: String, body: CF1LAE)

  type Value = Int
  type Env   = Map[String, Value]

  def interp(expr: CF1LAE, funDefs: Map[String, FunDef], env: Env): Value = expr match {
    case Num(n)        => n
    case Add(lhs, rhs) =>
      interp(lhs, funDefs, env) + interp(rhs, funDefs, env)
    case Sub(lhs, rhs) =>
      interp(lhs, funDefs, env) - interp(rhs, funDefs, env)
    case Mult(lhs, rhs) =>
      interp(lhs, funDefs, env) * interp(rhs, funDefs, env)
    case Let(boundId, namedExpr, boundBody) =>
      val newSubRep = env + (boundId -> interp(namedExpr, funDefs, env))
      interp(boundBody, funDefs, newSubRep)
    case If0(testExpr, thenExpr, elseExpr) =>
      val testV = interp(testExpr, funDefs, env)
      if testV == 0 then
          interp(thenExpr, funDefs, env)
      else
          interp(elseExpr, funDefs, env)

    case Id(name)              => env(name)
    case App(funName, argExpr) => funDefs(funName) match {
        case FunDef(argName, body) =>
          val funSubRep = Map(argName -> interp(argExpr, funDefs, env))
          interp(body, funDefs, funSubRep)
      }
  }
}
