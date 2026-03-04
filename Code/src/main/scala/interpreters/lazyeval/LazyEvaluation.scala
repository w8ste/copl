package interpreters.lazyeval

import scala.language.implicitConversions

object LazyEvaluation {
  sealed trait CFLAE
  case class Num(n: Int)                                      extends CFLAE
  case class Add(lhs: CFLAE, rhs: CFLAE)                      extends CFLAE
  case class Let(name: String, namedExpr: CFLAE, body: CFLAE) extends CFLAE
  case class Id(name: String)                                 extends CFLAE
  case class Fun(param: String, body: CFLAE)                  extends CFLAE
  case class App(funExpr: CFLAE, argExpr: CFLAE)              extends CFLAE
  case class If0(test: CFLAE, posBody: CFLAE, negBody: CFLAE) extends CFLAE

  // This object contains an interpreter for CFLAE/L with deferred substitution and
  // static scoping, which does never call strict at any time
  object NoStrict {

    type Env = Map[String, Value]

    case class FClosure(param: String, body: CFLAE, env: Env)
    case class EClosure(expr: CFLAE, env: Env)

    type Value = Num | FClosure | EClosure

    def interp(expr: CFLAE, env: Env = Map()): Value = expr match {
      case Num(n) => Num(n)

      case Add(lhs, rhs) =>
        val lhsV = interp(lhs, env)
        val rhsV = interp(rhs, env)
        (lhsV, rhsV) match {
          case (Num(n1), Num(n2)) => Num(n1 + n2)
          case _                  => sys.error("can only add numbers, but got: " + (lhsV, rhsV))
        }

      case Let(boundId, namedExpr, boundBody) =>
        interp(boundBody, env + (boundId -> EClosure(namedExpr, env)))

      case Id(name) => env(name)

      case Fun(arg, body) => FClosure(arg, body, env)

      case If0(testExpr, thenExpr, elseExpr) =>
        val testV = interp(testExpr, env)
        testV match {
          case Num(n) =>
            if n == 0 then
                interp(thenExpr, env)
            else
                interp(elseExpr, env)
          case _ => sys.error("can only test numbers, but got: " + testV)
        }

      case App(funExpr, argExpr) => interp(funExpr, env) match {
          case FClosure(fParam, fBody, fEnv) =>
            interp(fBody, fEnv + (fParam -> EClosure(argExpr, env)))
          case _ => sys.error("Can only apply closures")
        }
    }

    def strict(value: Value): Value = value match {
      case EClosure(expr, env) => strict(interp(expr, env))
      case _                   => value
    }
  }

  // This object contains an interpreter for CFLAE/L with deferred substitution and
  // static scoping. The interpreter is strict at function application,
  // arithmetic ops and conditionals; only identifier lookup is non-strict.
  object AlmostStrict {

    type Env = Map[String, Value]

    case class FClosure(param: String, body: CFLAE, env: Env)
    case class EClosure(expr: CFLAE, env: Env)

    type Value = Num | FClosure | EClosure

    def interp(expr: CFLAE, env: Env = Map()): Value = expr match {
      case Num(n) => Num(n)

      case Add(lhs, rhs) =>
        val lhsV = strict(interp(lhs, env))
        val rhsV = strict(interp(rhs, env))
        (lhsV, rhsV) match {
          case (Num(n1), Num(n2)) => Num(n1 + n2)
          case _                  => sys.error("can only add numbers, but got: " + (lhsV, rhsV))
        }

      case Let(boundId, namedExpr, boundBody) =>
        interp(boundBody, env + (boundId -> EClosure(namedExpr, env)))

      case Id(name) => env(name)

      case Fun(arg, body) => FClosure(arg, body, env)

      case If0(testExpr, thenExpr, elseExpr) =>
        val testV = strict(interp(testExpr, env))
        testV match {
          case Num(n) =>
            if n == 0 then
                interp(thenExpr, env)
            else
                interp(elseExpr, env)
          case _ => sys.error("can only test numbers, but got: " + testV)
        }

      case App(funExpr, argExpr) =>
        val funV = strict(interp(funExpr, env))
        funV match {
          case FClosure(fParam, fBody, fEnv) =>
            interp(fBody, fEnv + (fParam -> EClosure(argExpr, env)))
          case _ => sys.error("can only apply functions, but got: " + funV)
        }
    }

    def strict(value: Value): Value = value match {
      case EClosure(expr, env) => strict(interp(expr, env))
      case _                   => value
    }
  }

  // This file contains an interpreter for CFLAE/L with caching/thunks.
  object LazyThunks {

    type Env = Map[String, Value]

    case class FClosure(param: String, body: CFLAE, env: Env)
    case class EClosure(expr: CFLAE, env: Env)

    type Value = Num | FClosure | EClosure

    def interp(expr: CFLAE, env: Env = Map()): Value = expr match {
      case Num(n) => Num(n)

      case Add(lhs, rhs) =>
        val lhsV = strict(interp(lhs, env))
        val rhsV = strict(interp(rhs, env))
        (lhsV, rhsV) match {
          case (Num(n1), Num(n2)) => Num(n1 + n2)
          case _                  => sys.error("can only add numbers, but got: " + (lhsV, rhsV))
        }

      case Let(boundId, namedExpr, boundBody) =>
        interp(boundBody, env + (boundId -> EClosure(namedExpr, env)))

      case Id(name) => strict(env(name))

      case Fun(_, _) => EClosure(expr, env)

      case If0(testExpr, thenExpr, elseExpr) =>
        val testV = strict(interp(testExpr, env))
        testV match {
          case Num(n) =>
            if n == 0 then
                interp(thenExpr, env)
            else
                interp(elseExpr, env)
          case _ => sys.error("can only test numbers, but got: " + testV)
        }

      case App(funExpr, argExpr) =>
        val funV = strict(interp(funExpr, env))
        funV match {
          case EClosure(clExpr, clEnv) => clExpr match {
              case Fun(fParam, fBody) =>
                interp(fBody, clEnv + (fParam -> EClosure(argExpr, env)))
              case _ =>
                sys.error("expected function expression, but got: " + clExpr)
            }
          case _ =>
            sys.error("expected closure for application, but got: " + funV)
        }
    }

    def strict(value: Value): Value = value match {
      case EClosure(expr, env) => expr match {
          case Fun(_, _) => value
          case _         => strict(interp(expr, env))
        }
      case _ => value
    }
  }

  // This file contains an interpreter for CFLAE/L with caching
  object Caching {

    type Env = Map[String, Value]

    case class FClosure(param: String, body: CFLAE, env: Env)
    // here we use mutation to implement the cache
    // we could also choose to use Scala’s support for lazy values instead
    case class CacheClosure(expr: CFLAE, env: Env, var cache: Option[Value] = None)

    type Value = Num | FClosure | CacheClosure

    def interp(expr: CFLAE, env: Env = Map()): Value = expr match {
      case Num(n) => Num(n)

      case Add(lhs, rhs) =>
        val lhsV = strict(interp(lhs, env))
        val rhsV = strict(interp(rhs, env))
        (lhsV, rhsV) match {
          case (Num(n1), Num(n2)) => Num(n1 + n2)
          case _                  => sys.error("can only add numbers, but got: " + (lhsV, rhsV))
        }

      case Let(boundId, namedExpr, boundBody) =>
        interp(boundBody, env + (boundId -> CacheClosure(namedExpr, env)))

      case Id(name) => strict(env(name))

      case Fun(arg, body) => FClosure(arg, body, env)

      case If0(testExpr, thenExpr, elseExpr) =>
        val testV = strict(interp(testExpr, env))
        testV match {
          case Num(n) =>
            if n == 0 then
                interp(thenExpr, env)
            else
                interp(elseExpr, env)
          case _ => sys.error("can only test numbers, but got: " + testV)
        }

      case App(funExpr, argExpr) =>
        val funV = strict(interp(funExpr, env))
        funV match {
          case FClosure(fParam, fBody, fEnv) =>
            interp(fBody, fEnv + (fParam -> CacheClosure(argExpr, env)))
          case _ => sys.error("can only apply functions, but got: " + funV)
        }
    }

    def strict(value: Value): Value = value match {
      case e @ CacheClosure(expr, env, cache) =>
        cache match
            case None =>
              // println("Evaluate " + value)
              val res = strict(interp(expr, env))
              e.cache = Some(res)
              res
            case Some(cached) => cached
      case _ => value
    }
  }

  // This file contains an interpreter for CFLAE/L with caching
  object CachingInEnv {

    type Env = Map[String, Value]

    case class Result(env: Env, value: Value)

    sealed trait Value
    case class NumV(n: Int)                                   extends Value
    case class FClosure(param: String, body: CFLAE, env: Env) extends Value
    case class EClosure(expr: CFLAE, env: Env)                extends Value

    def interp(expr: CFLAE, env: Env = Map()): Result = expr match {
      case Num(n) => Result(env, NumV(n))

      case Add(lhs, rhs) =>
        val Result(leftEnv, lhsV)  = interp(lhs, env)
        val Result(rightEnv, rhsV) = interp(rhs, leftEnv)
        (lhsV, rhsV) match {
          case (NumV(n1), NumV(n2)) => Result(rightEnv, NumV(n1 + n2))
          case _                    => sys.error("can only add numbers, but got: " + (lhsV, rhsV))
        }

      case Let(boundId, namedExpr, boundBody) =>
        interp(boundBody, env + (boundId -> EClosure(namedExpr, env)))

      case Id(name) => strictEnv(name, env)

      case Fun(arg, body) => Result(env, FClosure(arg, body, env))

      case If0(testExpr, thenExpr, elseExpr) =>
        val testV = interp(testExpr, env)
        testV match {
          case Result(env2, NumV(n)) =>
            if n == 0 then
                interp(thenExpr, env2)
            else
                interp(elseExpr, env2)
          case _ => sys.error("can only test numbers, but got: " + testV)
        }

      case App(funExpr, argExpr) =>
        val funV = interp(funExpr, env)
        funV match {
          case Result(env2, FClosure(fParam, fBody, fEnv)) =>
            interp(fBody, env2 ++ fEnv + (fParam -> EClosure(argExpr, env2)))
          case _ => sys.error("can only apply functions, but got: " + funV)
        }
    }

    def strictEnv(id: String, environment: Env): Result = environment(id) match {
      case EClosure(expr, env) =>
        println("Evaluate " + id)
        val Result(env2, v) = interp(expr, env)
        Result(env2.updated(id, v), v)
      case x => Result(environment, x)
    }
  }

}
