package interpreters.rp

import scala.language.implicitConversions

trait TimeChanging {

  type Time

  sealed trait Expr
  case class Num(n: Int)                                    extends Expr
  case class Add(lhs: Expr, rhs: Expr)                      extends Expr
  case class Sub(lhs: Expr, rhs: Expr)                      extends Expr
  case class Mult(lhs: Expr, rhs: Expr)                     extends Expr
  case class Signum(expr: Expr)                             extends Expr
  case class Let(name: String, namedExpr: Expr, body: Expr) extends Expr
  case class Id(name: String)                               extends Expr
  case class If0(test: Expr, thenE: Expr, elseE: Expr)      extends Expr
  case class Fun(param: String, body: Expr)                 extends Expr
  case class App(funExpr: Expr, argExpr: Expr)              extends Expr

  // If you wanted to have this in an external DSL (not a DSL embedded into Scala or another language)
  // you would have many different expressions for different types of sources:
  // sensors, a clock, user input, network messages, ...
  // Here, we just allow to embed arbitrary “values” into the Expression
  case class Source(current: Value) extends Expr

  type Env = Map[String, Value]

  case class Closure(param: String, body: Expr, env: Env)

  type Value = Time => Num | Closure

  private def interpBinop(lhs: Expr, rhs: Expr, op: (Int, Int) => Int, env: Env): Time => Num =
      val lhv = interp(lhs, env)
      val rhv = interp(rhs, env)
      time =>
        (lhv(time), rhv(time)) match {
          case (Num(n1), Num(n2)) => Num(op(n1, n2))
          case v                  => sys.error(s"Expected numeric values but got $v")
        }

  def interp(expr: Expr, env: Env): Value = {
    expr match {

      // these represent number and function “primitives” which are defined in our language and don’t change over time
      case Num(n)           => time => Num(n)
      case Fun(param, body) => time => Closure(param, body, env)

      // the source expression allows us to inject external values into the system, which are the source of changes over time
      case Source(current) => current

      case Add(lhs, rhs) => interpBinop(lhs, rhs, _ + _, env)

      case Sub(lhs, rhs) => interpBinop(lhs, rhs, _ - _, env)

      case Mult(lhs, rhs) => interpBinop(lhs, rhs, _ * _, env)

      case Signum(expr) =>
        val value = interp(expr, env)
        time =>
          value(time) match
              case Num(n) => Num(Integer.signum(n))
              case other  => sys.error(s"can only compute the signum of numbers, but got $other")

      case If0(test, thenBody, elseBody) =>
        val testValue = interp(test, env)
        val thenValue = interp(thenBody, env)
        val elseValue = interp(elseBody, env)

        time =>
          testValue(time) match
              case Num(cond) => if cond == 0 then thenValue(time) else elseValue(time)
              case other     => sys.error(s"If0 requires a number but got: $other")

      case Let(boundId, namedExpr, boundBody) =>
        interp(boundBody, env + (boundId -> interp(namedExpr, env)))

      case Id(name) => env(name)

      case App(funExpr, argExpr) =>
        val funValue = interp(funExpr, env)
        val argVal   = interp(argExpr, env)
        time =>
          funValue(time) match {
            case Closure(param, body, funEnv) =>
              val extendedEnv: Env = funEnv + (param -> argVal)
              interp(body, extendedEnv)(time)

            case num: Num => sys.error(s"Expected function value but got $num")
          }
    }
  }
}
