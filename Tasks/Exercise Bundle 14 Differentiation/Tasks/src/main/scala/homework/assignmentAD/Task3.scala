package homework.assignmentAD

object Task3 {

  sealed trait FCRVF

  case class Num(n: Double)                                   extends FCRVF
  case class Add(lhs: FCRVF, rhs: FCRVF)                      extends FCRVF
  case class Mul(lhs: FCRVF, rhs: FCRVF)                      extends FCRVF
  case class Sub(lhs: FCRVF, rhs: FCRVF)                      extends FCRVF
  case class Sin(arg: FCRVF)                                  extends FCRVF
  case class Cos(arg: FCRVF)                                  extends FCRVF
  case class Exp(arg: FCRVF)                                  extends FCRVF
  case class Let(name: String, namedExpr: FCRVF, body: FCRVF) extends FCRVF
  case class Id(name: String)                                 extends FCRVF
  case class Fun(param: String, body: FCRVF)                  extends FCRVF
  case class App(funExp: FCRVF, arg: FCRVF)                   extends FCRVF

  class Ptr() // making use of that classes have a unique identity
  enum Tape:
      case Input(id: String)
      case Const(v: Double)
      case Add(lhs: Tape, rhs: Tape, ptr: Ptr)
      case Scale(v: Double, x: Tape, ptr: Ptr)

  case class VClosure(param: String, body: FCRVF, env: Env)
  case class Dual(x: Double, id: Tape)

  type Value = VClosure | Dual
  type Env   = Map[String, Value]

  def interp(expr: FCRVF, env: Env): Value = expr match {
    case num @ Num(x)  => Dual(x, Tape.Const(0))
    case Add(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (Dual(x1, t1), Dual(x2, t2)) => Dual(x1 + x2, Tape.Add(t1, t2, Ptr()))
        case _                            => sys.error(s"Can only add numbers, but got $lhs and $rhs")
      }
    case Sub(lhs, rhs) => ???

    case Mul(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (Dual(x1, t1), Dual(x2, t2)) =>
          Dual(x1 * x2, Tape.Add(Tape.Scale(x2, t1, Ptr()), Tape.Scale(x1, t2, Ptr()), Ptr()))
        case _ => sys.error(s"Can only multiply numbers, but got $lhs and $rhs")
      }
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

  def evalTapeFwd(t: Tape, env: Map[String, Double]): Double = t match
      case Tape.Input(id)        => env(id)
      case Tape.Const(v)         => v
      case Tape.Add(lhs, rhs, _) => evalTapeFwd(lhs, env) + evalTapeFwd(rhs, env)
      case Tape.Scale(v, x, _)   => v * evalTapeFwd(x, env)

  def evalTapeBwd(root: Tape, in: Double): Map[Tape, Double] =

      // collect nodes in postorder (evaluation order)
      def toposort(t: Tape, seen: Set[Ptr]): (List[Tape], Set[Ptr]) =
        t match
            case Tape.Add(l, r, p) =>
              if seen.contains(p) then (Nil, seen)
              else
                  val (lList, seen2) = toposort(l, seen + p)
                  val (rList, seen3) = toposort(r, seen2)
                  (t :: (lList ++ rList), seen3)
            case Tape.Scale(_, x, p) =>
              if seen.contains(p) then (Nil, seen)
              else
                  val (xList, seen2) = toposort(x, seen + p)
                  (t :: xList, seen2)
            case Tape.Input(x) => (Nil, seen)
            case Tape.Const(v) => (Nil, seen)

      val (order, _) = toposort(root, Set.empty)

      // initialize adjoint map with zeroes
      val adj = scala.collection.mutable.Map.empty[Tape, Double].withDefaultValue(0.0)
      // seed output derivative
      adj(root) = in

      // traverse nodes in reverse evaluation order
      for t <- order do
          t match
              case Tape.Add(l, r, _) =>
                adj(l) += adj(t)
                adj(r) += adj(t)
              case Tape.Scale(v, x, _) =>
                adj(x) += v * adj(t)
              case Tape.Input(_) => sys.error("should never happen")
              case Tape.Const(_) => sys.error("should never happen")

      // return immutable Map
      adj.toMap

  def derivative(expr: FCRVF, x: Double, varName: String = "x"): (Double, Double) = {
    val env: Env = Map(varName -> Dual(x, Tape.Input(varName)))
    interp(expr, env) match
        case Dual(value, tape) =>
          val adj   = evalTapeBwd(tape, 1.0)
          val deriv = adj.getOrElse(
            Tape.Input(varName),
            sys.error(s"Input $varName not in adjoint map")
          )
          (value, deriv)
        case _ => sys.error("should evaluate to a number with tape")
  }

  @main
  def testTapeTranspose(): Unit =
      println("fwd tape")
      // D(x^3)  ==  3*x^2
      def f(x: Double)  = x * x * x
      def df(x: Double) = 3 * x * x

      for x <- List.range(0, 10) yield
          val expected            = f(x)
          val expectedDer         = df(x)
          val (actual, actualDer) = derivative(Mul(Id("x"), Mul(Id("x"), Id("x"))), x)
          println(s"f($x): expected $expected $expectedDer || actual $actual $actualDer")

}
