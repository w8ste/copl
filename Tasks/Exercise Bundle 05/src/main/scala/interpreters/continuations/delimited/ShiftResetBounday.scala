package interpreters.continuations.delimited

import scala.util.boundary

object ShiftResetBoundary {

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

//  before
//  Reset {
//     ...
//     Shift(k => body)
//     next
//  }
//  after

  case class Closure(run: Continuation2 => (Continuation, Value) => Nothing)

  type Value = Num | Closure

  type Continuation  = Value => Nothing
  type Continuation2 = (Continuation, Value) => Nothing
  type Env           = Map[String, Value]

  /** Little helper for easier evaluation */
  def interp(e: KCFLAE): Value =
    boundary {
      interp(e, Map.empty, x => ???)((k, v) => boundary.break(v))
    }

  /** Abstract over left associative number operations */
  def binOp(
      verb: String,
      env: Env,
      envk1: Continuation,
      k: Continuation2,
      op: (Int, Int) => Int,
      lhs: KCFLAE,
      rhs: KCFLAE
  ): Nothing =
    interp(lhs, env, envk1) {
      (envk2, lv) =>
        interp(rhs, env, envk2) {
          (envk3, rv) =>
            (lv, rv) match
              case (Num(n1), Num(n2)) =>
                k(envk3, Num(op(n1, n2)))
              case _ =>
                sys.error(s"Can only $verb numbers, but got $lv and $rv")
        }
    }

  def interp(expr: KCFLAE, env: Env, envk1: Continuation)(k1: Continuation2): Nothing = expr match {
    case Num(n)         => k1(envk1, Num(n))
    case Add(lhs, rhs)  => binOp("add", env, envk1, k1, _ + _, lhs, rhs)
    case Sub(lhs, rhs)  => binOp("sub", env, envk1, k1, _ - _, lhs, rhs)
    case Mult(lhs, rhs) => binOp("mul", env, envk1, k1, _ * _, lhs, rhs)

    case Let(boundId, namedExpr, body) =>
      interp(namedExpr, env, envk1) {
        (envk2, nv) =>
          interp(body, env + (boundId -> nv), envk2) { k1 }
      }

    case Id(name) =>
      k1(envk1, env(name))

    case App(funExpr, argExpr) =>
      interp(funExpr, env, envk1) { (envk2, funValue) =>
        interp(argExpr, env, envk2) { (envk3, argValue) =>
          funValue match
            case Closure(clos) =>
              clos(k1)(envk3, argValue)
            case Num(n) =>
              sys.error(s"Can only apply closures or continuations, but got $funValue")
        }
      }

    case Fun(arg, body) =>
      k1(
        envk1,
        Closure(k2 =>
          (envk2, argValue) =>
            interp(body, env + (arg -> argValue), envk2)(k2)
        )
      )

    case If0(c, t, e) =>
      interp(c, env, envk1) { (envk2, res) =>
        val exp = res match
          case Num(0) => t
          case _      => e
        interp(exp, env, envk2)(k1)
      }

    case Reset(body) =>
      interp(body, env, v => k1(envk1, v))((k, v) => k(v))

    case Shift(name, body) =>
      // val value = Closure(k2 => k1.compose(k2))
      val value = Closure { k2 => (envk, v1) => k1(v2 => k2(envk, v2), v1) }
      interp(body, env + (name -> value), envk1)((k, v) => k(v))
  }
}
