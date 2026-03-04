package interpreters.differentiation

object ReversePure {

  sealed trait FCRV
  case class Num(n: Double)                                 extends FCRV
  case class Add(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Mul(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Sub(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Let(name: String, namedExpr: FCRV, body: FCRV) extends FCRV
  case class Id(name: String)                               extends FCRV
  case class Fun(param: String, body: FCRV)                 extends FCRV
  case class App(funExp: FCRV, arg: FCRV)                   extends FCRV
  // case class Diff(funExp: FCRV, arg: FCRV)                  extends FCRV

  type TapeId = Int
  case class TapeEntry(y: TapeId, d: Double, x: TapeId)
  type Tape = Vector[TapeEntry]

  case class VClosure(param: String, body: FCRV, env: Env)
  case class Dual(x: Double, id: TapeId)

  type Value = VClosure | Dual
  type Env   = Map[String, Value]

  // immutable state carrying next id and current tape
  case class State(nextId: TapeId, tape: Tape):
      def freshId: (TapeId, State) =
          val id = nextId
          (id, copy(nextId = nextId + 1))
      def append(te: TapeEntry): State = copy(tape = tape :+ te)

  // result type: value + new state
  case class WithState(v: Value, s: State)

  // entry point: interpret with initial state
  def interpTop(expr: FCRV, env: Env): WithState =
    interp(expr, env, State(1, Vector.empty))

  def interp(expr: FCRV, env: Env, state: State): WithState = expr match
      case Num(x) => WithState(Dual(x, 0), state)

      case Add(lhs, rhs) =>
        val WithState(lv, s1) = interp(lhs, env, state)
        val WithState(rv, s2) = interp(rhs, env, s1)
        (lv, rv) match
            case (Dual(x1, id1), Dual(x2, id2)) =>
              val (id, s3) = s2.freshId
              val s4       = s3.append(TapeEntry(id, 1.0, id1)).append(TapeEntry(id, 1.0, id2))
              WithState(Dual(x1 + x2, id), s4)
            case _ => sys.error(s"Can only add numbers, but got $lhs and $rhs")

      case Mul(lhs, rhs) =>
        val WithState(lv, s1) = interp(lhs, env, state)
        val WithState(rv, s2) = interp(rhs, env, s1)
        (lv, rv) match
            case (Dual(x1, id1), Dual(x2, id2)) =>
              val (id, s3) = s2.freshId
              val s4       = s3.append(TapeEntry(id, x2, id1)).append(TapeEntry(id, x1, id2))
              WithState(Dual(x1 * x2, id), s4)
            case _ => sys.error(s"Can only multiply numbers, but got $lhs and $rhs")

      case Sub(lhs, rhs) =>
        val WithState(lv, s1) = interp(lhs, env, state)
        val WithState(rv, s2) = interp(rhs, env, s1)
        (lv, rv) match
            case (Dual(x1, id1), Dual(x2, id2)) =>
              val (id, s3) = s2.freshId
              val s4       = s3.append(TapeEntry(id, 1.0, id1)).append(TapeEntry(id, -1.0, id2))
              WithState(Dual(x1 - x2, id), s4)
            case _ => sys.error(s"Can only subtract numbers, but got $lhs and $rhs")

      case Let(name, namedExpr, body) =>
        val WithState(argVal, s1) = interp(namedExpr, env, state)
        interp(body, env + (name -> argVal), s1)

      case Id(name) =>
        env.get(name) match
            case Some(v) => WithState(v, state)
            case None    => sys.error(s"unbound variable $name")

      case Fun(param, body) =>
        WithState(VClosure(param, body, env), state)

      case App(funExpr, argExpr) =>
        val WithState(funVal, s1) = interp(funExpr, env, state)
        funVal match
            case VClosure(param, body, funEnv) =>
              val WithState(argVal, s2) = interp(argExpr, env, s1)
              interp(body, funEnv + (param -> argVal), s2)
            case v1 => sys.error(s"Expected function value but got $v1")

      // case Diff(funExpr, argExpr) =>
      //   val VAndS(funVal, s1) = interp(funExpr, env, state)
      //   funVal match
      //     case VClosure(param, body, funEnv) =>
      //       val VAndS(argVal, s2) = interp(argExpr, env, s1)
      //       argVal match
      //         case Dual(x, _) =>
      //           // Start a fresh tape local to this differentiation
      //           val localStart = State(s2.nextId, Vector.empty)
      //           val (id1, s3) = localStart.freshId
      //           val extendedEnv = funEnv + (param -> Dual(x, id1))
      //           val VAndS(res, sAfter) = interp(body, extendedEnv, s3)
      //           res match
      //             case Dual(_, id2) =>
      //               // reverse accumulate gradients purely functionally
      //               val gradsInit: Map[TapeId, Double] = Map(id2 -> 1.0)
      //               val grads = sAfter.tape.reverse.foldLeft(gradsInit) { (gr, TapeEntry(y, d, x)) =>
      //                 val gy = gr.getOrElse(y, 0.0)
      //                 if gy == 0.0 then gr
      //                 else gr.updated(x, gr.getOrElse(x, 0.0) + gy * d)
      //               }
      //               val resultGrad = grads.getOrElse(id1, 0.0)
      //               VAndS(Dual(resultGrad, 0), s2) // return to outer state s2 (no tape carried out)
      //             case v3 => sys.error(s"Expected function to return number but got $v3")
      //         case v2 => sys.error(s"Expected number but got $v2")
      //     case v1 => sys.error(s"Expected function value but got $v1")

  def evaluateTape(tape: Tape, base: Map[TapeId, Double], target: TapeId): Double =
      // build a map from y -> list of (d, x) edges
      val edges: Map[TapeId, Vector[(Double, TapeId)]] =
        tape.foldLeft(Map.empty[TapeId, Vector[(Double, TapeId)]]) { case (m, TapeEntry(y, d, x)) =>
          m.updated(y, m.getOrElse(y, Vector.empty) :+ (d -> x))
        }
      // recursive evaluator with memoization to avoid recomputation
      def eval(id: TapeId, memo: Map[TapeId, Double]): (Double, Map[TapeId, Double]) =
        if memo.contains(id) then (memo(id), memo)
        else
            edges.get(id) match
                case None => // leaf: take from base map (default 0.0)
                  val v = base.getOrElse(id, 0.0)
                  (v, memo.updated(id, v))
                case Some(children) =>
                  // Value of y = sum_d d * value(x)
                  val (sum, memo2) = children.foldLeft((0.0, memo)) { case ((acc, mm), (d, x)) =>
                    val (vx, mm2) = eval(x, mm)
                    (acc + d * vx, mm2)
                  }
                  (sum, memo2.updated(id, sum))
      eval(target, Map.empty)._1

  // convenience: compute value for the final id (last entry's y) using given leaf values
  def execTape(tape: Tape, leafValues: Map[TapeId, Double]): Option[Double] =
    if tape.isEmpty then None
    else
        val finalY = tape.last.y
        Some(evaluateTape(tape, leafValues, finalY))

  @main def testReversePure(): Unit =
      println("fwd tape")
      // D(x^3)  ==  3*x^2
      def f(x: Double)  = x * x * x
      def df(x: Double) = 3 * x * x

      for x <- List.range(0, 10) yield
          val expected                              = f(x)
          val expectedDer                           = df(x)
          val WithState(actualBoth, State(_, tape)) =
            interp(Mul(Id("x"), Mul(Id("x"), Id("x"))), Map.empty + ("x" -> Dual(x, 0)), State(1, Vector())) // match
          val (actual, id) = actualBoth match
              case Dual(x, id)                => (x, id)
              case VClosure(param, body, env) => sys.error("unexpected")
          val actualDer = execTape(tape, Map(0 -> 1.0))
          println(s"f($x): expected $expected $expectedDer || actual $actual $actualDer")
          // case VAndS(v, state) =>
          //   val actualFwd = interp(tape, Map.empty + ("x" -> 1.0))
          //   val actualBwd = interp(tape, 1.0)(Tape.Input("x"))
          //   println(s"f($x): expected $expected $expectedDer || actual $actual $actualFwd $actualBwd")
          // case _ => sys.error("should evaluate to a number with tape")
}
