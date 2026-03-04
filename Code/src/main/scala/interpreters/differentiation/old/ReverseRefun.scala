package interpreters.differentiation.old

object ReverseRefun {

  sealed trait FCRV

  case class Num(n: Double)                                 extends FCRV
  case class Add(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Mul(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Sub(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Let(name: String, namedExpr: FCRV, body: FCRV) extends FCRV
  case class Id(name: String)                               extends FCRV
  case class Fun(param: String, body: FCRV)                 extends FCRV
  case class App(funExp: FCRV, arg: FCRV)                   extends FCRV
  case class Diff(funExp: FCRV, arg: FCRV)                  extends FCRV
  // TODO: case class Diff2(exp: FCRV, vars: Array[String])          extends FCRV

  case class VClosure(param: String, body: FCRV, env: Env)
  case class Dual(x: Double, var d: Double)

  type Value = VClosure | Dual
  type Env   = Map[String, Value]

  var tape = () => ()

  def interp(expr: FCRV, env: Env): Value = expr match {
    case num @ Num(x)  => Dual(x, 0)
    case Add(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (lv @ Dual(x1, _), rv @ Dual(x2, _)) =>
          val y       = Dual(x1 + x2, 0)
          val oldTape = tape
          tape = () => { lv.d += y.d; rv.d += y.d; oldTape() }
          y
        case _ => sys.error(s"Can only add numbers, but got $lhs and $rhs")
      }
    case Mul(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (lv @ Dual(x1, _), rv @ Dual(x2, _)) =>
          val y       = Dual(x1 * x2, 0)
          val oldTape = tape
          tape = () => { lv.d += x2 * y.d; rv.d += x1 * y.d; oldTape() }
          y
        case _ => sys.error(s"Can only multiply numbers, but got $lhs and $rhs")
      }
    case Sub(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (lv @ Dual(x1, _), rv @ Dual(x2, _)) =>
          val y       = Dual(x1 - x2, 0)
          val oldTape = tape
          tape = () => { lv.d += y.d; rv.d -= y.d; oldTape() }
          y
        case _ => sys.error(s"Can only subtract numbers, but got $lhs and $rhs")
      }
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
    case Diff(funExpr, argExpr) =>
      interp(funExpr, env) match {
        case VClosure(param, body, funEnv) =>
          interp(argExpr, env) match {
            case Dual(x, _) =>
              tape = () => ()
              val z           = Dual(x, 0)
              val extendedEnv = funEnv + (param -> z)
              interp(body, extendedEnv) match {
                case v @ Dual(_, _) =>
                  v.d = 1.0
                  tape()
                  Dual(z.d, 0)
                case v3 => sys.error(s"Expected function to return number bot got $v3")
              }
            case v2 => sys.error(s"Expected number but got $v2")
          }
        case v1 => sys.error(s"Expected function value but got $v1")
      }
  }

  @main
  def RVRtest(): Unit =
    println(interp(Diff(Fun("x", Mul(Id("x"), Mul(Id("x"), Id("x")))), Num(4)), Map.empty))
}
