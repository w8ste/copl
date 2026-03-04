package modularized.environmental

import modularized.{Expressions, isEnvironmentOf}

import scala.compiletime.deferred

trait RecursiveEnvironment[E, Value] extends (E isEnvironmentOf Value) {
  extension (env: E)
      /** bind recursive expects to be passed a mutable value that is initially null,
        * but then changed to the actual value before a lookup happens
        */
      def bindRecursive(id: String, value: => Value | Null): E
      override def updated(id: String, value: Value): E = bindRecursive(id, value)
}

trait Recursion extends BaseInterp, Expressions {

  override given valueEnvironment: RecursiveEnvironment[Env, Value] = deferred

  def interpRecursion(expr: LetRec, env: Env): Value = expr match
      case LetRec(boundId, namedExpr, boundBody) =>
        var mutableValue: Value | Null = null
        val recEnv                     = env.bindRecursive(boundId, mutableValue)
        val namedValue                 = interp(namedExpr, recEnv)
        mutableValue = namedValue
        interp(boundBody, recEnv)
}
