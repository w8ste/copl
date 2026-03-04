package interpreters.continuations

import scala.annotation.tailrec

object ExplicitStackContinuations {

  sealed trait ContinuationsCFLAE
  // For reasons unknown to me, continuations are traditionally bound to an identifier called `k`.
  // My guess would be that it is due to the large influence of the german speaking community on certain math communities.
  // The K in KCFLAE is from that k – and to disambiguate it better from CFLAE.
  type KCFLAE = ContinuationsCFLAE
  case class Num(n: Int)                                      extends KCFLAE
  case class Add(lhs: KCFLAE, rhs: KCFLAE)                    extends KCFLAE
  case class Id(name: String)                                 extends KCFLAE
  case class Fun(param: String, body: KCFLAE)                 extends KCFLAE
  case class App(funKCFLAE: KCFLAE, arg: KCFLAE)              extends KCFLAE
  case class If0(testE: KCFLAE, thenE: KCFLAE, elseE: KCFLAE) extends KCFLAE
  case class BindCC(name: String, body: KCFLAE)               extends KCFLAE
  case object Hole                                            extends KCFLAE

  case class Closure(param: String, body: KCFLAE, env: Env)
  case class Continuation(returnValues: List[Value], frames: List[StackFrame])

  case class StackFrame(term: KCFLAE, env: Env)

  type Value = Num | Closure | Continuation

  type Env = Map[String, Value]

  def interp(term: KCFLAE): Value = interp(term, Map.empty, Nil, Nil)

  @tailrec
  def interp(term: KCFLAE, env: Env, returnValues: List[Value], frames: List[StackFrame]): Value = {

    // println(s"evaluating:\n\t$term\n\t$env\n\t$returnValues\n\t$frames")

    inline def handleValue(v: Value, retVals: List[Value]) =
      frames match
          case Nil                                              => v
          case StackFrame(nextExpr, nextEnv) :: remainingFrames =>
            // println(s"pushing $v")
            interp(nextExpr, nextEnv, v :: retVals, remainingFrames)

    term match
        case Id(id)           => handleValue(env(id), returnValues)
        case num: Num         => handleValue(num, returnValues)
        case Fun(param, body) => handleValue(
            Closure(param, body, env),
            returnValues
          )

        case Add(Hole, Hole) =>
          returnValues match
              case Num(r) :: Num(l) :: rest => handleValue(Num(l + r), rest)
              case other                    => sys.error(s"not enough values to fill all holes in: $term")
        case Add(Hole, rhs) =>
          interp(rhs, env, returnValues, StackFrame(Add(Hole, Hole), env) :: frames)
        case Add(lhs, rhs) =>
          interp(lhs, env, returnValues, StackFrame(Add(Hole, rhs), env) :: frames)

        case If0(Hole, thenE, elseE) =>
          returnValues match
              case Num(n) :: rest =>
                if n == 0
                then interp(thenE, env, rest, frames)
                else interp(elseE, env, rest, frames)
              case other => sys.error(s"not enough values to fill all holes in: $term")
        case If0(testE, thenE, elseE) =>
          interp(testE, env, returnValues, StackFrame(If0(Hole, thenE, elseE), env) :: frames)

        case App(Hole, Hole) =>
          returnValues match
              case (arg: Value) :: Closure(param, body: KCFLAE, cEnv) :: rest =>
                interp(body, cEnv.updated(param, arg), rest, frames)
              case (arg: Value) :: Continuation(contValues, contStack) :: _ =>
                contStack match
                    case Nil                           => arg
                    case StackFrame(term, env) :: rest => interp(term, env, arg :: contValues, rest)
              case other => sys.error(s"can not apply ${returnValues}")
        case App(Hole, arg: KCFLAE) =>
          interp(arg, env, returnValues, StackFrame(App(Hole, Hole), env) :: frames)
        case App(funKCFLAE: KCFLAE, arg: KCFLAE) =>
          interp(funKCFLAE, env, returnValues, StackFrame(App(Hole, arg), env) :: frames)

        case BindCC(name, body: KCFLAE) =>
          interp(body, env + (name -> Continuation(returnValues, frames)), returnValues, frames)

        case Hole => sys.error("Found a Hole where it was not expected")
  }

}
