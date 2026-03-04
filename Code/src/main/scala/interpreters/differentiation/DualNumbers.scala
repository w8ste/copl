package interpreters.differentiation

object DualNumbers {

  sealed trait FCDN

  case class Num(n: Double)                                 extends FCDN
  case class Add(lhs: FCDN, rhs: FCDN)                      extends FCDN
  case class Mul(lhs: FCDN, rhs: FCDN)                      extends FCDN
  case class Sub(lhs: FCDN, rhs: FCDN)                      extends FCDN
  case class Let(name: String, namedExpr: FCDN, body: FCDN) extends FCDN
  case class Id(name: String)                               extends FCDN
  case class Fun(param: String, body: FCDN)                 extends FCDN
  case class App(funExp: FCDN, arg: FCDN)                   extends FCDN
  // case class Diff(funExp: FCDN, arg: FCDN)                  extends FCDN

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
    case Mul(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (Dual(x1, d1), Dual(x2, d2)) => Dual(x1 * x2, x1 * d2 + x2 * d1)
        case _                            => sys.error(s"Can only multiply numbers, but got $lhs and $rhs")
      }
    case Sub(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (Dual(x1, d1), Dual(x2, d2)) => Dual(x1 - x2, d1 - d2)
        case _                            => sys.error(s"Can only subtract numbers, but got $lhs and $rhs")
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
    // case Diff(funExpr, argExpr) =>
    //   interp(funExpr, env) match {
    //     case VClosure(param, body, funEnv) =>
    //       interp(argExpr, env) match {
    //         case Dual(x, _) =>
    //           val extendedEnv: Env = funEnv + (param -> Dual(x, 1))
    //           interp(body, extendedEnv) match {
    //             case Dual(_, d) => Dual(d, 0)
    //             case v3         => sys.error(s"Expected function to return number bot got $v3")
    //           }
    //         case v2 => sys.error(s"Expected number but got $v2")
    //       }
    //     case v1 => sys.error(s"Expected function value but got $v1")
    //   }
  }

  @main
  def testDualNumbers(): Unit =
      println("dual number")
      // D(x^3)  ==  3*x^2
      def f(x: Double)  = x * x * x
      def df(x: Double) = 3 * x * x

      for x <- List.range(0, 10) yield
          val expected    = f(x)
          val expectedDer = df(x)
          val actualBoth  = interp(Mul(Id("x"), Mul(Id("x"), Id("x"))), Map.empty + ("x" -> Dual(x, 1.0))) // match
          val (actual, actualDer) = actualBoth match
              case Dual(x, id)                => (x, id)
              case VClosure(param, body, env) => sys.error("unexpected")
          println(s"f($x): expected $expected $expectedDer || actual $actual $actualDer")
}
