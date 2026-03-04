package modularized.environmental

import modularized.Expressions

trait Functions extends BaseInterp, Expressions {

  case class Closure(param: String, body: Expr, env: Env)

  def interpFunctions(expr: FunExpr | Let, env: Env): Value | Closure = expr match {

    case Let(boundId, namedExpr, boundBody) =>
      interp(boundBody, env.updated(boundId, interp(namedExpr, env)))

    case Id(name) => env.lookup(name)

    case Fun(param, body) =>
      Closure(param, body, env)

    case App(funExpr, argExpr) => interp(funExpr, env) match {
        case Closure(param, body, funEnv) =>
          val argVal           = interp(argExpr, env)
          val extendedEnv: Env = funEnv.updated(param, argVal)
          interp(body, extendedEnv)

        case v1 => sys.error(s"Expected function value but got $v1")
      }
  }

}
