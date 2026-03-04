package interpreters.continuations.delimited

// We can implement delimited continuations,
// without using boundary/break in the host language, e.g.,
// in any pure language:

object ShiftResetPurely {

  // A Functional Abstraction of Typed Contexts
  // https://plv.mpi-sws.org/plerg/papers/danvy-filinski-89-2up.pdf

  // Final Shift for Call/cc:
  // https://www.deinprogramm.de/sperber/papers/shift-reset-direct.pdf

  sealed trait KCFLAE
  case class Num(n: Int)                                      extends KCFLAE
  case class Add(lhs: KCFLAE, rhs: KCFLAE)                    extends KCFLAE
  case class Sub(lhs: KCFLAE, rhs: KCFLAE)                    extends KCFLAE
  case class Mult(lhs: KCFLAE, rhs: KCFLAE)                   extends KCFLAE
  case class Let(name: String, namedE: KCFLAE, body: KCFLAE)  extends KCFLAE
  case class Id(name: String)                                 extends KCFLAE
  case class Fun(param: String, body: KCFLAE)                 extends KCFLAE
  case class App(funExpr: KCFLAE, arg: KCFLAE)                extends KCFLAE
  case class If0(testE: KCFLAE, thenE: KCFLAE, elseE: KCFLAE) extends KCFLAE
  case class Shift(name: String, body: KCFLAE)                extends KCFLAE
  case class Reset(body: KCFLAE)                              extends KCFLAE

  case class Closure(run: Continuation2 => Continuation2) // a closure
  type Value = Num | Closure

  type Continuation  = Value => Value // a continuation takes a value of any type x and returns a value of that type
  type Continuation2 = Continuation => Continuation
  type Env           = Map[String, Value]

  /** Little helper for easier evaluation */
  def interp(e: KCFLAE): Value =
    // interpretation starts with the identity continuation and identity meta continuation
    interp(e, Map.empty, (x: Value) => x)((x: Continuation) => x)

  /** Abstract over left associative number operations */
  def binOp(
      verb: String,
      env: Env,
      y: Continuation,
      k: Continuation2,
      op: (Int, Int) => Int,
      lhs: KCFLAE,
      rhs: KCFLAE
  ): Value =
    // Note: it is possible to syntactically get rid of these nested callbacks by having the interpreters return a
    // “suspended computation” – a data structure that awaits a continuation.
    // However, doing so requires to use more abstractions distracting from what we want to teach.
    interp(lhs, env, y) {
      y2 => lv =>
        interp(rhs, env, y2) {
          y3 => rv =>
            (lv, rv) match
              case (Num(n1), Num(n2)) =>
                k(y3)(Num(op(n1, n2)))
              case _ =>
                sys.error(s"Can only $verb numbers, but got $lv and $rv")
        }
    }

  // Instead of returning a value, the result is passed to the continuation k.
  def interp(expr: KCFLAE, env: Env, envk1: Continuation)(k1: Continuation2): Value = expr match {
    // for the cases without subexpressions,
    // the value and the continuation is always passed to the metacontinuation
    case Num(n)   => k1(envk1)(Num(n))
    case Id(name) =>
      k1(envk1)(env(name))
    case Fun(arg, body) =>
      k1(envk1)(Closure(k2 =>
        y2 =>
          v2 =>
            interp(body, env + (arg -> v2), y2)(k2)
      ))

    // in the cases with subexpressions,
    // the continuation is threaded through the interpreter,
    // while enlargening the metacontinuation with the current term
    case Add(lhs, rhs)                 => binOp("add", env, envk1, k1, _ + _, lhs, rhs)
    case Sub(lhs, rhs)                 => binOp("sub", env, envk1, k1, _ - _, lhs, rhs)
    case Mult(lhs, rhs)                => binOp("mul", env, envk1, k1, _ * _, lhs, rhs)
    case Let(boundId, namedExpr, body) =>
      interp(namedExpr, env, envk1) {
        envk2 => nv =>
          interp(body, env + (boundId -> nv), envk2) { k1 }
      }
    case App(funExpr, argExpr) =>
      interp(funExpr, env, envk1) { envk2 => funValue =>
        interp(argExpr, env, envk2) { envk3 => argValue =>
          funValue match
            case Closure(clos) =>
              clos(k1)(envk3)(argValue)
            case Num(n) =>
              sys.error(s"Can only apply closures or continuations, but got $funValue")
        }
      }

    case If0(c, t, e) =>
      interp(c, env, envk1) { envk2 => res =>
        val exp = res match
          case Num(0) => t
          case _      => e
        interp(exp, env, envk2)(k1)
      }

    // reset merges the metacontinuation k into the continuation envk
    // (setting the new metacontinuation to identity)
    case Reset(body) =>
      interp(body, env, k1(envk1))(identity)

    // shift moves the metacontinuation to the given variable name
    // (setting the new metacontinuation to identity)
    case Shift(name, body) =>
      val valu = Closure(k2 => k1.compose(k2))
      interp(body, env + (name -> valu), envk1)(identity)
  }
}
