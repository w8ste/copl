package homework.assignment06state

import scala.language.implicitConversions

object Task3Until0 {

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
  case class Until0(untilExpr: SCFLAE, body: SCFLAE)             extends SCFLAE

  type Location = Int

  type Env = Map[String, Location]

  case class Box(location: Location)
  case class Closure(param: String, body: SCFLAE, env: Env)

  type Value = Num | Closure | Box

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

    case Until0(untilExpr, body) => ???

  }
}
