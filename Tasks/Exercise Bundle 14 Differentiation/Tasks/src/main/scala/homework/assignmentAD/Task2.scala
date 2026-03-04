package homework.assignmentAD

object Task2 {
  sealed trait FCDN

  case class Num(n: Double)                                 extends FCDN
  case class Add(lhs: FCDN, rhs: FCDN)                      extends FCDN
  case class Mul(lhs: FCDN, rhs: FCDN)                      extends FCDN
  case class Sub(lhs: FCDN, rhs: FCDN)                      extends FCDN
  case class Sin(arg: FCDN)                                 extends FCDN
  case class Cos(arg: FCDN)                                 extends FCDN
  case class Exp(arg: FCDN)                                 extends FCDN
  case class Let(name: String, namedExpr: FCDN, body: FCDN) extends FCDN
  case class Id(name: String)                               extends FCDN
  case class Fun(param: String, body: FCDN)                 extends FCDN
  case class App(funExp: FCDN, arg: FCDN)                   extends FCDN

  case class VClosure(param: String, body: FCDN, env: Env)
  case class Dual(x: Double, d: Double)

  type Value = VClosure | Dual
  type Env   = Map[String, Value]

  def interp(expr: FCDN, env: Env): Value = expr match {
    case num @ Num(x)  => Dual(x, 0)
    case Add(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (Dual(x1, d1), Dual(x2, d2)) => Dual(x1 + x2, d1 + d2)
        case _                            => sys.error(s"Can only add numbers, but got $lhs and $rhs")
      }
    case Mul(lhs, rhs) => ???

    case Sub(lhs, rhs) => ???

    case Sin(arg) => ???

    case Cos(arg) => ???

    case Exp(arg) => ???

    case Let(boundId, namedExpr, boundBody) =>
      interp(boundBody, env + (boundId -> interp(namedExpr, env)))
    case Id(name)              => env.getOrElse(name, sys.error(s"unbound variable $name"))
    case Fun(param, body)      => VClosure(param, body, env)
    case App(funExpr, argExpr) =>
      interp(funExpr, env) match {
        case VClosure(param, body, funEnv) =>
          val argVal           = interp(argExpr, env)
          val extendedEnv: Env = funEnv + (param -> argVal)
          interp(body, extendedEnv)
        case v1 => sys.error(s"Expected function value but got $v1")
      }
  }

}
