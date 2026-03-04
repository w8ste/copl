package interpreters.memory

import interpreters.memory.stores.Store

object GarbageCollection {

  sealed trait StoreExpr
  case class Num(n: Int)                                                  extends StoreExpr
  case class Add(lhs: StoreExpr, rhs: StoreExpr)                          extends StoreExpr
  case class Mult(lhs: StoreExpr, rhs: StoreExpr)                         extends StoreExpr
  case class Let(name: String, namedExpr: StoreExpr, body: StoreExpr)     extends StoreExpr
  case class Id(name: String)                                             extends StoreExpr
  case class If0(test: StoreExpr, posBody: StoreExpr, negBody: StoreExpr) extends StoreExpr
  case class Fun(param: String, body: StoreExpr)                          extends StoreExpr
  case class LetRec(name: String, namedExpr: StoreExpr, body: StoreExpr)  extends StoreExpr
  case class App(funExpr: StoreExpr, argExpr: StoreExpr)                  extends StoreExpr
  case class Seqn(e1: StoreExpr, e2: StoreExpr)                           extends StoreExpr
  case class SetId(id: String, valueExpr: StoreExpr)                      extends StoreExpr
  case class NewBox(valExpr: StoreExpr)                                   extends StoreExpr
  case class SetBox(boxExpr: StoreExpr, valueExpr: StoreExpr)             extends StoreExpr
  case class OpenBox(boxExpr: StoreExpr)                                  extends StoreExpr

  type Env = Map[String, Location]

  case class Closure(param: String, body: StoreExpr, env: Env)
  case class Box(location: Location)

  type Value = Num | Closure | Box

  def locationsInValue(v: Value): Set[Location] = v match {
    case Box(a)             => Set(a)
    case Num(_)             => Set()
    case Closure(_, _, env) => env.values.toSet
  }

  /* In our interpreter, the stack of environments is only implicitly
   * available on the stack of the meta-language. To reify the call-
   * stack we need to make it explicit. We do so by constructing the
   * stack explicitly encoded in the `RootedEnv`
   */
  def interp(
      expr: StoreExpr,
      env: RootedEnv,
      store: Store[Value]
  ): (Value, Store[Value]) = {

    def binOp(lhs: StoreExpr, rhs: StoreExpr, combine: (Int, Int) => Int): (Value, Store[Value]) =
        val (lv, s1) = interp(lhs, env, store)
        val (rv, s2) = interp(rhs, env, s1)
        (lv, rv) match {
          case (Num(m), Num(n)) => (Num(combine(m, n)), s2)
          case _                => sys.error(s"Can do arthmetic with numbers, but got $lhs and $rhs")
        }

    expr match {

      case Num(n) => (Num(n), store)

      case Add(lhs, rhs)  => binOp(lhs, rhs, _ + _)
      case Mult(lhs, rhs) => binOp(lhs, rhs, _ * _)

      case Let(boundId, namedExpr, boundBody) =>
        val (namedVal, s1) = interp(namedExpr, env, store)
        val (newLoc, s2)   = s1.malloc(namedVal, env.roots)
        val newEnv         = env + (boundId -> newLoc)
        interp(boundBody, newEnv, s2)

      case Id(name) => (store.lookup(env(name)), store)

      case Fun(arg, body) => (Closure(arg, body, env.dict), store)

      case If0(testExpr, thenExpr, elseExpr) =>
        val (testV, s1) = interp(testExpr, env, store)
        testV match {
          case Num(n) =>
            if n == 0 then interp(thenExpr, env, s1)
            else interp(elseExpr, env, s1)
          case _ => sys.error("can only test numbers, but got: " + testV)
        }

      /** In our stateful language, we do not require mutation from the
        * host language to implement cyclic environments.
        */
      case LetRec(boundId, namedExpr, boundBody) =>
        val (newLoc, s2)          = store.malloc(Num(0), env.roots)
        val extEnv                = env + (boundId -> newLoc)
        val (namedVal, bodyStore) = interp(namedExpr, extEnv, s2)
        interp(boundBody, extEnv, bodyStore.update(newLoc, namedVal))

      case App(funExpr, argExpr) =>
        val (funV, funStore) = interp(funExpr, env, store)
        val (argV, argStore) = interp(argExpr, env, funStore)
        funV match {
          case Closure(fParam, fBody, fEnv) =>
            val (newLoc, resStore) = argStore.malloc(argV, env.roots)
            interp(fBody, env.replace(fEnv.updated(fParam, newLoc)), resStore)
          case _ => sys.error(s"can only apply functions, but got: $funV")
        }

      case Seqn(e1, e2) =>
        val (l1, s1) = interp(e1, env, store)
        interp(e2, env, s1)

      case NewBox(boxExpr) =>
        val (boxV, s1)  = interp(boxExpr, env, store)
        val (boxVl, s2) = s1.malloc(boxV, env.roots)
        (Box(boxVl), s2)

      case SetBox(boxExpr, valueExpr) =>
        val (boxV, s1)  = interp(boxExpr, env, store)
        val (value, s2) = interp(valueExpr, env, s1)
        boxV match {
          case Box(loc) => (value, s2.update(loc, value))
          case _        => sys.error(s"can only set to boxes, but got: $boxV")
        }

      case OpenBox(boxExpr) =>
        val (boxV, s1) = interp(boxExpr, env, store)
        boxV match {
          case Box(loc) => (store.lookup(loc), s1)
          case _        => sys.error(s"can only open boxes, but got: $boxV")
        }

      case SetId(id, valExpr) =>
        val (value, s1) = interp(valExpr, env, store)
        (value, s1.update(env(id), value))
    }
  }
}
