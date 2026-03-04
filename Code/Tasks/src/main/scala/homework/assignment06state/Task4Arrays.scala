package homework.assignment06state

import scala.language.implicitConversions

object Task4Arrays {
  sealed trait ArraySCFLAE
  type ASCFLAE = ArraySCFLAE
  case class Num(n: Int)                                                           extends ASCFLAE
  case class Add(lhs: ASCFLAE, rhs: ASCFLAE)                                       extends ASCFLAE
  case class Let(name: String, namedExpr: ASCFLAE, body: ASCFLAE)                  extends ASCFLAE
  case class Id(name: String)                                                      extends ASCFLAE
  case class If0(test: ASCFLAE, posBody: ASCFLAE, negBody: ASCFLAE)                extends ASCFLAE
  case class Fun(param: String, body: ASCFLAE)                                     extends ASCFLAE
  case class App(funExpr: ASCFLAE, argExpr: ASCFLAE)                               extends ASCFLAE
  case class Seqn(e1: ASCFLAE, e2: ASCFLAE)                                        extends ASCFLAE
  case class SetId(id: String, valueExpr: ASCFLAE)                                 extends ASCFLAE
  case class NewArray(size: ASCFLAE)                                               extends ASCFLAE
  case class SetArrayIndex(arrayExpr: ASCFLAE, index: ASCFLAE, valueExpr: ASCFLAE) extends ASCFLAE
  case class GetArrayIndex(arrayExpr: ASCFLAE, index: ASCFLAE)                     extends ASCFLAE

  type Location = Int

  type Env = Map[String, Location]

  case class Closure(param: String, body: ASCFLAE, env: Env)

  case object Undefined
  // note, we do shadow the built in Array type here … that’s fine :-)
  case class Array(location: Location, size: Int)

  type Value = Num | Closure | Undefined.type | Array

  type Store = Map[Location, Value]

  def nextLocation(store: Store): Location = store.keys.maxOption.getOrElse(0) + 1

  def interp(expr: ASCFLAE): (Value, Store) = interp(expr, Map.empty, Map.empty)

  def interp(
      expr: ASCFLAE,
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

    case SetId(id, valExpr) =>
      val (value, s1) = interp(valExpr, env, store)
      (value, s1 + (env(id) -> value))

    case NewArray(sizeExpr) => ???

    // interpretation order: 1. arrayExpr, 2. indexExpr, 3. valueExpr
    case SetArrayIndex(arrayExpr, indexExpr, valueExpr) => ???

    // interpretation order: 1. arrayExpr, 2. indexExpr
    case GetArrayIndex(arrayExpr, indexExpr) => ???

  }
}
