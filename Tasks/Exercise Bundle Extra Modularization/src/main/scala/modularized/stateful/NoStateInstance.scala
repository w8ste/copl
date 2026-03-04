package modularized.stateful

import modularized.isEnvironmentOf

object NoStateInstance extends Functions with Arithmetic {

  type Value = Num | Closure

  type Expr = ArithmeticExpr | FunExpr

  type Env = Map[String, Value]

  type Context[+A] = A

  override given sequentialContext: Sequential[Context] with {
    override def ret[V](v: V): Context[V] = v
    extension [A](m: Context[A])
        override def andThen[B](f: A => Context[B]): Context[B] = f(m)
  }

  override given storeEnv: EnvironmentStoreApi[Value, Env, Context] = new EnvironmentStoreApi[Value, Env, Context] {
    extension (env: Env) {
      override def bind(name: String, value: Value): Env   = env.updated(name, value)
      override def lookup(name: String): Value             = env.apply(name)
      override def stack(other: Env): Env                  = env
      override def reachable(other: Value): Env            = env
      override def update(name: String, value: Value): Env = env.bind(name, value)
    }
  }

  override def interp(expr: Expr, env: Env): Context[Value] = expr match
      case expr: ArithmeticExpr => interpArithmetic(expr, env)
      case expr: FunExpr        => interpFunctions(expr, env)

  given locationEnvironment: (Env isEnvironmentOf Value) with {
    extension (env: Env)
        override def lookup(id: String): Value              = env.apply(id)
        override def updated(id: String, value: Value): Env = env.updated(id, value)
  }

}
