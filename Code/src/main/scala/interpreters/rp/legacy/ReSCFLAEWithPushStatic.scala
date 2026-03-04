package interpreters.rp.legacy

import interpreters.memory.stores.MinimalStore
import modularized.stateful.Location

import java.util.Date
import scala.language.implicitConversions

object ReSCFLAEWithPushStatic {
  sealed trait ReSCFLAE
  case class Num(n: Int)                                               extends ReSCFLAE
  case class Add(lhs: ReSCFLAE, rhs: ReSCFLAE)                         extends ReSCFLAE
  case class Sub(lhs: ReSCFLAE, rhs: ReSCFLAE)                         extends ReSCFLAE
  case class Let(name: String, namedExpr: ReSCFLAE, body: ReSCFLAE)    extends ReSCFLAE
  case class Id(name: String)                                          extends ReSCFLAE
  case class If0(test: ReSCFLAE, posBody: ReSCFLAE, negBody: ReSCFLAE) extends ReSCFLAE
  case class Fun(param: String, body: ReSCFLAE)                        extends ReSCFLAE
  case class App(funExpr: ReSCFLAE, argExpr: ReSCFLAE)                 extends ReSCFLAE
  case class Seqn(e1: ReSCFLAE, e2: ReSCFLAE)                          extends ReSCFLAE
  case class NewVar(valExpr: ReSCFLAE)                                 extends ReSCFLAE
  case class SetVar(varExpr: ReSCFLAE, valueExpr: ReSCFLAE)            extends ReSCFLAE
  case class CurrVal(varExpr: ReSCFLAE)                                extends ReSCFLAE
  case class Time()                                                    extends ReSCFLAE
  case class Wait(millis: Int)                                         extends ReSCFLAE

  // First two entries are reserved for time and its dependencies
  val (initialStore, timeLoc, timeDepLoc) =
      val s1        = MinimalStore[Value](100)
      val (tl, s2)  = s1.malloc(Num(0))
      val (tdl, s3) = s2.malloc(VarDependencies())
      (s3, tl, tdl)

  type Env   = Map[String, Location]
  type Store = MinimalStore[Value]

  case class Closure(param: String, body: ReSCFLAE, env: Env)
  case class Var(valLoc: Location, exprLoc: Location, depLoc: Location)
  case class VarDependencies(dependencies: List[Value] = List())

  type Value = Num | Closure | Var | VarDependencies

  // Traverses the dependency tree in post-order and returns a reversed linearization (-> Reverse post-order)
  def orderDependencies(dependencies: List[Value], store: Store): List[Value] = {
    dependencies.foldLeft(List[Value]())((prev, nextDep) =>
      (nextDep match {
        case Var(_, _, depLoc) => store.lookup(depLoc) match {
            case VarDependencies(dep) => prev ::: orderDependencies(dep, store)
            case _                    => sys.error("Invalid dependency list: " + depLoc)
          }
        case _ => sys.error("Can only order dependencies for vars, but got: " + nextDep)
      }) :+ nextDep
    ).distinct.reverse
  }

  // Adds a dependency link between two vars
  def addDependency(fromVar: Value, toVar: Value, store: Store): Store = {
    fromVar match {
      case Var(_, _, depLoc) => store.lookup(depLoc) match {
          case VarDependencies(depList) => store.update(depLoc, VarDependencies(depList :+ toVar))
          case _                        => sys.error("Invalid dependency list: " + depLoc)
        }
      case _ => sys.error("Can only add dependencies to vars, but got: " + fromVar)
    }
  }

  // Updates the dependencies stored at the given location in the store
  def updateDependencies(depLoc: Location, store: Store): Store = {
    store.lookup(depLoc) match {
      case VarDependencies(dependencies) =>
        val updateSchedule = orderDependencies(dependencies, store)
        val sNew           = updateSchedule.foldLeft(store)((sPrev, dep) =>
          dep match {
            case Var(depValLoc, depExprLoc, depDepLoc) =>
              sPrev.lookup(depExprLoc) match {
                case Closure(_, depExprBody, depExprEnv) =>
                  val (depValNew, depSNew) = interp(depExprBody, depExprEnv, sPrev)
                  depSNew.update(depValLoc, depValNew)
                case _ => sys.error("Encountered non-closure value as var expression: " + dep)
              }
            case _ => sys.error("Encountered non-var value in dependency list: " + dep)
          }
        )
        sNew
      case _ => sys.error("Invalid dependency list: " + depLoc)
    }
  }

  def updateTime(store: Store): Store = {
    val s1 = store
      .update(timeLoc, Num(new Date().getTime.toInt))
    updateDependencies(timeDepLoc, s1)
  }

  def interp(
      expr: ReSCFLAE,
      env: Env = Map(),
      storeOld: Store = initialStore
  ): (Value, Store) = {
    val store = updateTime(storeOld)

    expr match {
      case Num(n) => (Num(n), store)

      case Add(lhs, rhs) =>
        val (lhsv, s1) = interp(lhs, env, store)
        val (rhsv, s2) = interp(rhs, env, s1)

        (lhsv, rhsv) match {
          case (Var(_, _, _), Var(_, _, _)) =>
            val (newVar, s3) = interp(NewVar(Add(CurrVal(lhs), CurrVal(rhs))), env, s2)
            val s4           = addDependency(lhsv, newVar, addDependency(rhsv, newVar, s3))
            (newVar, s4)
          case (Var(_, _, _), _) =>
            val (newVar, s3) = interp(NewVar(Add(CurrVal(lhs), rhs)), env, s2)
            val s4           = addDependency(lhsv, newVar, s3)
            (newVar, s4)
          case (_, Var(_, _, _)) =>
            val (newVar, s3) = interp(NewVar(Add(lhs, CurrVal(rhs))), env, s2)
            val s4           = addDependency(rhsv, newVar, s3)
            (newVar, s4)
          case (Num(n1), Num(n2)) => (Num(n1 + n2), s2)
          case _                  => sys.error(
              "can only add numbers, but got: %s and %s".format(lhsv, rhsv)
            )
        }

      case Sub(lhs, rhs) =>
        val (lhsv, s1) = interp(lhs, env, store)
        val (rhsv, s2) = interp(rhs, env, s1)

        (lhsv, rhsv) match {
          case (Var(_, _, _), Var(_, _, _)) =>
            val (newVar, s3) = interp(NewVar(Sub(CurrVal(lhs), CurrVal(rhs))), env, s2)
            val s4           = addDependency(lhsv, newVar, addDependency(rhsv, newVar, s3))
            (newVar, s4)
          case (Var(_, _, _), _) =>
            val (newVar, s3) = interp(NewVar(Sub(CurrVal(lhs), rhs)), env, s2)
            val s4           = addDependency(lhsv, newVar, s3)
            (newVar, s4)
          case (_, Var(_, _, _)) =>
            val (newVar, s3) = interp(NewVar(Sub(lhs, CurrVal(rhs))), env, s2)
            val s4           = addDependency(rhsv, newVar, s3)
            (newVar, s4)
          case (Num(n1), Num(n2)) => (Num(n1 - n2), s2)
          case _                  => sys.error(
              "can only subtract numbers, but got: %s and %s".format(lhsv, rhsv)
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
          case Var(_, _, _) =>
            val (thenV, s2) = interp(thenExpr, env, s1)
            thenV match {
              case Var(_, _, _) =>
                val (elseV, s3) = interp(elseExpr, env, s2)
                elseV match {
                  case Var(_, _, _) =>
                    val (newVar, s4) =
                      interp(NewVar(If0(CurrVal(testExpr), CurrVal(thenExpr), CurrVal(elseExpr))), env, s3)
                    val s5 =
                      addDependency(testV, newVar, addDependency(thenV, newVar, addDependency(elseV, newVar, s4)))
                    (newVar, s5)
                  case _ =>
                    val (newVar, s4) = interp(NewVar(If0(CurrVal(testExpr), CurrVal(thenExpr), elseExpr)), env, s2)
                    val s5           = addDependency(testV, newVar, addDependency(thenV, newVar, s4))
                    (newVar, s5)
                }
              case _ =>
                val (elseV, s3) = interp(elseExpr, env, s1)
                elseV match {
                  case Var(_, _, _) =>
                    val (newVar, s4) = interp(NewVar(If0(CurrVal(testExpr), thenExpr, CurrVal(elseExpr))), env, s3)
                    val s5           = addDependency(testV, newVar, addDependency(elseV, newVar, s4))
                    (newVar, s5)
                  case _ =>
                    val (newVar, s4) = interp(NewVar(If0(CurrVal(testExpr), thenExpr, elseExpr)), env, s1)
                    val s5           = addDependency(testV, newVar, s4)
                    (newVar, s5)
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
        val (funV, s1) = interp(funExpr, env, store)

        funV match {
          case Var(_, _, _) =>
            val (newVar, s2) = interp(NewVar(App(CurrVal(funExpr), argExpr)), env, s1)
            val s3           = addDependency(funV, newVar, s2)
            (newVar, s3)
          case Closure(fParam, fBody, fEnv) =>
            val (argV, s2)   = interp(argExpr, env, s1)
            val (newLoc, s3) = s2.malloc(argV)
            interp(fBody, fEnv + (fParam -> newLoc), s3)
          case _ => sys.error("can only apply functions, but got: " + funV)
        }

      case Seqn(e1, e2) =>
        val (_, s1) = interp(e1, env, store)
        interp(e2, env, s1)

      case NewVar(varExpr) =>
        // Compute the initial value of the var
        val (varVal, _)  = interp(varExpr, env, store)
        val (valLoc, s1) = store.malloc(varVal)
        // And save the uninterpreted expression as closure for later updates
        val varBody       = Closure("_", varExpr, env)
        val (bodyLoc, s2) = s1.malloc(varBody)
        // Location where later dependencies are stored (initally empty)
        val (depLoc, s3) = s2.malloc(VarDependencies())

        (
          Var(valLoc, bodyLoc, depLoc),
          s3
        )

      case SetVar(varExpr, valueExpr) =>
        val (varV, s1) = interp(varExpr, env, store)
        // Compute new values for the var ...
        val (valueV, s2) = interp(valueExpr, env, s1)
        val varBody      = Closure("_", valueExpr, env)

        varV match {
          case Var(valLoc, exprLoc, depLoc) =>
            // ... update the var in the store
            val s3 = s2.update(valLoc, valueV).update(exprLoc, varBody)

            // ... and update all dependent vars
            val s4 = updateDependencies(depLoc, s3)
            (varV, s4)
          case _ => sys.error("can only set to vars, but got: " + varV)
        }

      case CurrVal(valExpr) =>
        val (valV, s1) = interp(valExpr, env, store)
        valV match {
          // For Vars, CurrVal extracts the current content of the var
          case Var(valLoc, _, _) =>
            (s1.lookup(valLoc), s1)
          // For all other values, it just returns the value itself
          case x => (x, s1)
        }

      case Time() =>
        (Var(timeLoc, timeLoc, timeDepLoc), store)

      case Wait(millis) =>
        Thread.sleep(millis)
        (Num(millis), store)
    }
  }
}
