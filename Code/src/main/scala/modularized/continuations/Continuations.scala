package modularized.continuations

import modularized.isEnvironmentOf
import modularized.stateful.{Arithmetic, EnvironmentStoreApi, Functions, Sequential}

object Continuations extends Functions with Arithmetic {

  case class Continuation(c: Value => Nothing)

  type Value = Num | Closure | Continuation

  type Expr = ArithmeticExpr | FunExpr | ContinuationExpr

  type Env = Map[String, Value]

  type Context[+A] = (A => Nothing) => Nothing

  override given sequentialContext: Sequential[Context] with {
    override def ret[V](v: V): Context[V] = k => k(v)
    extension [A](m: Context[A])
        override def andThen[B](f: A => Context[B]): Context[B] = k => m(a => f(a).apply(k))
  }

  override given storeEnv: EnvironmentStoreApi[Value, Env, Context] = new EnvironmentStoreApi[Value, Env, Context] {
    extension (env: Env) {
      override def bind(name: String, value: Value): Context[Env]   = Sequential.ret(env.updated(name, value))
      override def lookup(name: String): Context[Value]             = Sequential.ret(env.apply(name))
      override def stack(other: Env): Env                           = env
      override def reachable(other: Value): Env                     = env
      override def update(name: String, value: Value): Context[Env] = Sequential.ret(env.updated(name, value))

    }
  }

  override def interp(expr: Expr, env: Env): Context[Value] = expr match
      case expr: ArithmeticExpr => interpArithmetic(expr, env)
      case expr: FunExpr        => interpFunctions(expr, env)
      case BindCC(name, body)   =>
        (k: Value => Nothing) =>
          interp(body, env.updated(name, Continuation(k))).apply(k)
      case AppK(funExpr, argExpr) =>
        for {
          fv   <- interp(funExpr, env)
          argV <- interp(argExpr, env)
        } yield {
          fv match
              case Continuation(c) => c(argV)
              case _               =>
                sys.error(s"Can only apply closures or continuations, but got $fv")

        }

  given (Env isEnvironmentOf Value) with {
    extension (env: Env)
        override def lookup(id: String): Value              = env.apply(id)
        override def updated(id: String, value: Value): Env = env.updated(id, value)
  }

}
