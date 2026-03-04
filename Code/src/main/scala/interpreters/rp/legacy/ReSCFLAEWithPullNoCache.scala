package interpreters.rp.legacy

import interpreters.memory.stores.MinimalStore
import modularized.stateful.Location

import scala.language.implicitConversions

object ReSCFLAEWithPullNoCache {
  sealed trait ReSCFLAE
  case class Num(n: Int)                                               extends ReSCFLAE
  case class Add(lhs: ReSCFLAE, rhs: ReSCFLAE)                         extends ReSCFLAE
  case class Let(name: String, namedExpr: ReSCFLAE, body: ReSCFLAE)    extends ReSCFLAE
  case class Id(name: String)                                          extends ReSCFLAE
  case class If0(test: ReSCFLAE, posBody: ReSCFLAE, negBody: ReSCFLAE) extends ReSCFLAE
  case class Fun(param: String, body: ReSCFLAE)                        extends ReSCFLAE
  case class App(funExpr: ReSCFLAE, argExpr: ReSCFLAE)                 extends ReSCFLAE
  case class Seqn(e1: ReSCFLAE, e2: ReSCFLAE)                          extends ReSCFLAE
  case class NewVar(valExpr: ReSCFLAE)                                 extends ReSCFLAE
  case class SetVar(varExpr: ReSCFLAE, valueExpr: ReSCFLAE)            extends ReSCFLAE
  case class CurrVal(varExpr: ReSCFLAE)                                extends ReSCFLAE

  implicit def idToSCFLAE(id: String): Id = Id(id)
  implicit def numToSCFLAE(n: Int): Num   = Num(n)

  type Env   = Map[String, Location]
  type Store = MinimalStore[Value]

  case class Closure(param: String, body: ReSCFLAE, env: Env)
  case class Var(location: Location)

  type Value = Num | Closure | Var

  def interp(
      expr: ReSCFLAE,
      env: Env = Map(),
      store: Store = MinimalStore(100)
  ): (Value, Store) = {
    expr match {

      case Num(n) => (Num(n), store)

      case Add(lhs, rhs) =>
        val (lhsv, s1) = interp(lhs, env, store)
        val (rhsv, s2) = interp(rhs, env, s1)

        // When adding vars, don’t use the interpreted result but simply construct a new var around them
        (lhsv, rhsv) match {
          case (Var(_), Var(_))   => interp(NewVar(Add(CurrVal(lhs), CurrVal(rhs))), env, store)
          case (Var(_), _)        => interp(NewVar(Add(CurrVal(lhs), rhs)), env, store)
          case (_, Var(_))        => interp(NewVar(Add(lhs, CurrVal(rhs))), env, store)
          case (Num(n1), Num(n2)) => (Num(n1 + n2), s2)
          case _                  => sys.error(
              "can only add numbers, but got: %s and %s".format(lhsv, rhsv)
            )
        }

      case Let(boundId, namedExpr, boundBody) =>
        val (namedVal, s1) = interp(namedExpr, env, store)
        val (newLoc, s2)   = s1.malloc(namedVal)
        interp(boundBody, env + (boundId -> newLoc), s2)

      case Id(name) => (store.lookup(env(name)), store)

      case Fun(arg, body) => (Closure(arg, body, env), store)

      case If0(testExpr, thenExpr, elseExpr) =>
        val (testV, s1) = interp(testExpr, env, store)

        testV match {
          case Var(_) =>
            val (thenV, s2) = interp(thenExpr, env, s1)
            thenV match {
              case Var(_) =>
                val (elseV, s3) = interp(elseExpr, env, s2)
                elseV match {
                  case Var(_) => interp(NewVar(If0(CurrVal(testExpr), CurrVal(thenExpr), CurrVal(elseExpr))), env, s3)
                  case _      => interp(NewVar(If0(CurrVal(testExpr), CurrVal(thenExpr), elseExpr)), env, s2)
                }
              case _ =>
                val (elseV, s3) = interp(elseExpr, env, s1)
                elseV match {
                  case Var(_) => interp(NewVar(If0(CurrVal(testExpr), thenExpr, CurrVal(elseExpr))), env, s3)
                  case _      => interp(NewVar(If0(CurrVal(testExpr), thenExpr, elseExpr)), env, s1)
                }
            }
          case Num(n) =>
            if n == 0 then {
              interp(thenExpr, env, s1)
            } else {
              interp(elseExpr, env, s1)
            }
          case _ => sys.error("can only test numbers, but got: " + testV)
        }

      case App(funExpr, argExpr) =>
        val (funV, s1)       = interp(funExpr, env, store)
        val (argV, argStore) = interp(argExpr, env, s1)

        funV match {
          case Var(_)                       => interp(NewVar(App(CurrVal(funExpr), argExpr)), env, store)
          case Closure(fParam, fBody, fEnv) =>
            val (newLoc, s1) = argStore.malloc(argV)
            interp(fBody, fEnv + (fParam -> newLoc), s1)
          case _ => sys.error("can only apply functions, but got: " + funV)
        }

      case Seqn(e1, e2) =>
        val (_, s1) = interp(e1, env, store)
        interp(e2, env, s1)

      case NewVar(varExpr) =>
        // Create closure around varExpr ...
        val varBody      = Closure("_", varExpr, env)
        val (newLoc, s1) = store.malloc(varBody)
        // ... and store it at the location saved in Var
        (Var(newLoc), s1)

      case SetVar(varExpr, valueExpr) =>
        val (varV, s1) = interp(varExpr, env, store)
        val varBody    = Closure("_", valueExpr, env)

        varV match {
          case Var(loc) => (varV, s1.update(loc, varBody))
          case _        => sys.error("can only set to vars, but got: " + varV)
        }

      case CurrVal(valExpr) =>
        val (valV, s1) = interp(valExpr, env, store)
        valV match {
          // For Vars, CurrVal extracts the current content of the var ...
          case Var(loc) =>
            s1.lookup(loc) match {
              case Closure(_, body, vEnv) =>
                // ... and executes it
                interp(body, vEnv, s1)
              case _ => sys.error(s"invalid content of var: ${s1.lookup(loc)}")
            }
          // For all other values, it just returns the value itself
          case x => (x, s1)
        }
    }
  }
}
