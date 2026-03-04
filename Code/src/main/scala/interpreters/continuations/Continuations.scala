package interpreters.continuations

import scala.util.boundary

object Continuations {

  sealed trait ContinuationsCFLAE
  // For reasons unknown to me, continuations are traditionally bound to an identifier called `k`.
  // My guess would be that it is due to the large influence of the german speaking community on certain math communities.
  // The K in KCFLAE is from that k – and to disambiguate it better from CFLAE.
  type KCFLAE = ContinuationsCFLAE
  case class Num(n: Int)                                      extends KCFLAE
  case class Msg(s: String)                                   extends KCFLAE
  case class Add(lhs: KCFLAE, rhs: KCFLAE)                    extends KCFLAE
  case class Sub(lhs: KCFLAE, rhs: KCFLAE)                    extends KCFLAE
  case class Mult(lhs: KCFLAE, rhs: KCFLAE)                   extends KCFLAE
  case class Let(name: String, namedE: KCFLAE, body: KCFLAE)  extends KCFLAE
  case class Id(name: String)                                 extends KCFLAE
  case class Fun(param: String, body: KCFLAE)                 extends KCFLAE
  case class App(funExpr: KCFLAE, arg: KCFLAE)                extends KCFLAE
  case class If0(testE: KCFLAE, thenE: KCFLAE, elseE: KCFLAE) extends KCFLAE
  case class BindCC(name: String, body: KCFLAE)               extends KCFLAE

  case class Closure(param: String, body: KCFLAE, env: Env)
  case class Continuation(c: Value => Nothing)

  type Value = Num | Closure | Continuation | Msg

  type Env = Map[String, Value]

  /** Little helper for easier evaluation */
  def interp(e: KCFLAE): Value =
    // boundary/break is a little library function to “break out” of the block delimited by `boundary` returning the value passed to break.
    // This works perfectly to convert from the continuation passing world back to the normal value returning world.
    boundary {
      Continuations.interp(e, Map.empty, x => boundary.break(x))
    }

  // Instead of returning a value, the result is passed to the continuation k.
  def interp(expr: KCFLAE, env: Env, k: Value => Nothing): Nothing = {

    /** Abstract over left associative number operations */
    def binOp(verb: String, op: (Int, Int) => Int, lhs: KCFLAE, rhs: KCFLAE): Nothing =
      // Note: it is possible to syntactically get rid of these nested callbacks by having the interpreters return a
      // “suspended computation” – a data structure that awaits a continuation.
      // However, doing so requires to use more abstractions distracting from what we want to teach.
      interp(
        lhs,
        env,
        lv =>
          interp(
            rhs,
            env,
            rv =>
              (lv, rv) match
                  case (Num(n1), Num(n2)) => k(Num(op(n1, n2)))
                  case _                  =>
                    sys.error(s"Can only $verb numbers, but got $lv and $rv")
          )
      )

    expr match {
      case Num(n)         => k(Num(n))
      case Msg(m)         => k(Msg(m))
      case Add(lhs, rhs)  => binOp("add", _ + _, lhs, rhs)
      case Sub(lhs, rhs)  => binOp("subtract", _ - _, lhs, rhs)
      case Mult(lhs, rhs) => binOp("multiply", _ * _, lhs, rhs)

      case Let(boundId, namedExpr, body) =>
        interp(namedExpr, env, nv => interp(body, env + (boundId -> nv), k))

      case Id(name) => k(env(name))

      case App(funExpr, argExpr) =>
        interp(
          funExpr,
          env,
          fv =>
            interp(
              argExpr,
              env,
              argV =>
                fv match {
                  case Closure(param, body, funEnv) =>
                    interp(body, funEnv + (param -> argV), k)
                  case Continuation(c) => c(argV)
                  case _               =>
                    sys.error(s"Can only apply closures or continuations, but got $fv")
                }
            )
        )

      case Fun(arg, body) => k(Closure(arg, body, env))

      case If0(c, t, e) =>
        interp(
          c,
          env,
          res =>
            res match
                case Num(0) => interp(t, env, k)
                case _      => interp(e, env, k)
        )

      case BindCC(name, body) =>
        interp(body, env + (name -> Continuation(k)), k)
    }
  }
}
