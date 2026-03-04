package modularized.environmental

import modularized.isEnvironmentOf

trait BaseInterp {

  /** Abstract types here allow us to combine different interpreters, by specialising these values when we mix the traits together. */
  type Expr

  type Value

  type Env

  /** [[isEnvironmentOf]] is just a trait which is written using infix syntax, this is equivalent to:
    * `isEnvironmentOf[Env, Value]`
    * The `given` means that the instance of this trait will be available implicitly.
    * In our case this just means that we can use the extension methods defined in that trait, which allow us to treat the left type as “an environment of” the right type.
    *
    * We use this encoding (as opposed to requiring Env to be of a specific type) to allow us to use existing types (i.e., Scala’s Map) directly as environments.
    */
  given valueEnvironment: (Env isEnvironmentOf Value)

  def interp(expr: Expr, env: Env): Value

}
