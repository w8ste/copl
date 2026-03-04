package homework.assignment10bmodularization

import modularized.environmental.{Booleans, Functions, Records}
import modularized.isEnvironmentOf

object ModularInterpreter extends Functions, Booleans, Records {

  override type Env = Map[String, Value]

  def interp(expr: Expr): Value = interp(expr, Map.empty)

  override def interp(expr: Expr, env: Env): Value = ???

  given valueEnvironment: (Env isEnvironmentOf Value) with {
    extension (env: Env) {
      def lookup(id: String): Value =
        ???

      def updated(id: String, value: Value): Env =
        ???

    }
  }
}
