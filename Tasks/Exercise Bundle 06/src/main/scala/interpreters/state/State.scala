package interpreters.state

// This file contains an interpreter for SCFLAE with non-recursive first-class
// functions, conditionals, mutable boxes, variables and sequencing.
object State {

  sealed trait StateCFLAE
  type SCFLAE = StateCFLAE
  case class Num(n: Int)                                         extends SCFLAE
  case class Add(lhs: SCFLAE, rhs: SCFLAE)                       extends SCFLAE
  case class Let(name: String, namedExpr: SCFLAE, body: SCFLAE)  extends SCFLAE
  case class Id(name: String)                                    extends SCFLAE
  case class If0(test: SCFLAE, posBody: SCFLAE, negBody: SCFLAE) extends SCFLAE
  case class Fun(param: String, body: SCFLAE)                    extends SCFLAE
  case class App(funExpr: SCFLAE, argExpr: SCFLAE)               extends SCFLAE
  case class Seqn(e1: SCFLAE, e2: SCFLAE)                        extends SCFLAE
  case class SetId(id: String, valueExpr: SCFLAE)                extends SCFLAE
  case class NewBox(valExpr: SCFLAE)                             extends SCFLAE
  case class SetBox(boxExpr: SCFLAE, valueExpr: SCFLAE)          extends SCFLAE
  case class OpenBox(boxExpr: SCFLAE)                            extends SCFLAE

  /** ReFun case is only used in the second interpreter.
    * We already define it as part of SCFLAE so we don’t have to duplicate the AST definition
    */
  case class ReFun(param: String, body: SCFLAE) extends SCFLAE

  type Location = Int

  type Env = Map[String, Location]

  case class Box(location: Location)
  case class Closure(param: String, body: SCFLAE, env: Env)

  trait Base {

    type Value >: Num | Closure | Box
    type Store = Map[Location, Value]

    def nextLocation(store: Store): Location = store.keys.maxOption.getOrElse(0) + 1

    def interp(expr: SCFLAE): (Value, Store) = interp(expr, Map.empty, Map.empty)

    def interp(
        expr: SCFLAE,
        env: Env,
        store: Store
    ): (Value, Store) = expr match {

      case Num(n) => (Num(n), store)

      case Add(lhs, rhs) =>
        val (lv, s1) = interp(lhs, env, store)
        val (rv, s2) = interp(rhs, env, s1)
        (lv, rv) match {
          case (Num(m), Num(n)) => (Num(m + n), s2)
          case _                => sys.error(s"Can only do arithmetic with numbers, but got $lhs and $rhs")
        }

      case Let(boundId, namedExpr, boundBody) =>
        val (namedVal, s1) = interp(namedExpr, env, store)
        val newLoc         = nextLocation(s1)
        interp(boundBody, env + (boundId -> newLoc), s1 + (newLoc -> namedVal))

      case Id(name) => (store(env(name)), store)

      case Fun(arg, body) => (Closure(arg, body, env), store)

      case If0(testExpr, thenExpr, elseExpr) =>
        val (testV, s1) = interp(testExpr, env, store)
        testV match {
          case Num(n) =>
            if n == 0 then interp(thenExpr, env, s1)
            else interp(elseExpr, env, s1)
          case _ => sys.error("can only test numbers, but got: " + testV)
        }

      case App(funExpr, argExpr) =>
        val (funV, funStore) = interp(funExpr, env, store)
        val (argV, argStore) = interp(argExpr, env, funStore)
        funV match {
          case Closure(fParam, fBody, fEnv) =>
            val newLoc = nextLocation(argStore)
            interp(fBody, fEnv + (fParam -> newLoc), argStore + (newLoc -> argV))
          case _ => sys.error("can only apply functions, but got: " + funV)
        }

      case Seqn(e1, e2) =>
        val (_, s1) = interp(e1, env, store)
        interp(e2, env, s1)

      case NewBox(boxExpr) =>
        val (boxV, boxStore) = interp(boxExpr, env, store)
        val newLoc           = nextLocation(boxStore)
        (Box(newLoc), boxStore + (newLoc -> boxV))

      case SetBox(boxExpr, valueExpr) =>
        val (boxV, s1)  = interp(boxExpr, env, store)
        val (value, s2) = interp(valueExpr, env, s1)
        boxV match {
          case Box(loc) => (value, s2 + (loc -> value))
          case _        => sys.error("can only set to boxes, but got: " + boxV)
        }

      case OpenBox(boxExpr) =>
        val (boxV, s1) = interp(boxExpr, env, store)
        boxV match {
          case Box(loc) => (s1(loc), s1)
          case _        => sys.error("can only open boxes, but got: " + boxV)
        }

      case SetId(id, valExpr) =>
        val (value, s1) = interp(valExpr, env, store)
        (value, s1 + (env(id) -> value))

      case unhandled: ReFun => sys.error("The base interpreter does not support call-by-reference functions")
    }
  }

  /** Just extends base without further modifications, thus implementing the standard call-by-value semantics most of our interpreters use. */
  object Standard extends Base {
    override type Value = Num | Closure | Box
  }

  /** Extends the interpreter with the necessary logic for functions that take their arguments by reference.
    * Note that we do not modify semantics of existing functions, thus this interpreter allows to specify per function/argument if it should be passed by value or by reference.
    */
  object CallByReference extends Base {

    case class RefClosure(param: String, body: SCFLAE, env: Env)
    override type Value = RefClosure | Closure | Num | Box

    override def interp(
        expr: SCFLAE,
        env: Env,
        store: Store
    ): (Value, Store) = expr match {

      case ReFun(arg, body) => (RefClosure(arg, body, env), store)

      case App(funExpr, argExpr) =>
        val (funV, funStore) = interp(funExpr, env, store)
        funV match {
          case RefClosure(fParam, fBody, fEnv) => argExpr match {
              case Id(argName) =>
                interp(fBody, fEnv + (fParam -> env(argName)), funStore)
              case _ => sys.error("can only call identifiers by reference")
            }
          case _ => super.interp(expr, env, store)
        }
      case other => super.interp(expr, env, store)
    }
  }
}
