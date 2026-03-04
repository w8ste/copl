package interpreters.differentiation.old

object ReverseRefun2 {

  sealed trait FCRV

  case class Num(n: Double)                                 extends FCRV
  case class Add(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Mul(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Sub(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Let(name: String, namedExpr: FCRV, body: FCRV) extends FCRV
  case class Id(name: String)                               extends FCRV
  case class Fun(param: String, body: FCRV)                 extends FCRV
  case class App(funExp: FCRV, arg: FCRV)                   extends FCRV
  // Diff case removed as requested

  case class VClosure(param: String, body: FCRV, env: Env)
  case class Dual(x: Double, var d: Double)

  type Value = VClosure | Dual
  type Env   = Map[String, Value]

  // Tape is global for simplicity, mirroring original design
  var tape: () => Unit = () => ()

  def interp(expr: FCRV, env: Env): Value = expr match {
    case Num(x)        => Dual(x, 0)
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
  }

  def differentiate(expr: FCRV, inputs: Map[String, Double]): (Double, Map[String, Double]) = {
    tape = () => ()
    val inputDuals = inputs.map { case (k, v) => k -> Dual(v, 0.0) }
    val result     = interp(expr, inputDuals)
    result match {
      case resDual @ Dual(resVal, _) =>
        resDual.d = 1.0
        tape()
        val gradients = inputDuals.map { case (k, d) => k -> d.d }
        (resVal, gradients)
      case _ => sys.error("Differentiation result must be a number")
    }
  }

  @main
  def RVRtest2(): Unit = {
    val term            = Add(Mul(Id("x"), Id("y")), Id("x"))
    val (result, grads) = differentiate(term, Map("x" -> 3.0, "y" -> 4.0))

    println(s"Expression: x * y + x where x=3.0, y=4.0")
    println(s"Result: $result (Expected: 15.0)")
    println(s"Gradient x: ${grads("x")} (Expected: 5.0)")
    println(s"Gradient y: ${grads("y")} (Expected: 3.0)")
  }
}
