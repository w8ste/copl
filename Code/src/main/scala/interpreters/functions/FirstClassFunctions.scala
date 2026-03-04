package interpreters.functions

object FirstClassFunctions {

  sealed trait FirstClassLAE
  type FLAE = FirstClassLAE

  case class Num(n: Int)                                    extends FLAE
  case class Add(lhs: FLAE, rhs: FLAE)                      extends FLAE
  case class Sub(lhs: FLAE, rhs: FLAE)                      extends FLAE
  case class Let(name: String, namedExpr: FLAE, body: FLAE) extends FLAE
  case class Id(name: String)                               extends FLAE
  case class Fun(param: String, body: FLAE)                 extends FLAE
  case class App(funExp: FLAE, arg: FLAE)                   extends FLAE

  /** This use some of the intermediate level Scala features to remove some of the redundancy in the interpreter,
    * specifically the basic cases for arithmetic are always the same, so we extract them into the `interpAE`` method.
    * However, this method needs to call the (abstract) `interp` method, which is implemented by the subclasses,
    * to make sure that any advanced features within the arithmetic expressions are handled as they should.
    */
  trait Arithmetic {
    type Env
    type Value >: Num
    type AE = Num | Sub | Add
    def interp(expr: FLAE, env: Env): Value
    def interpAE(expr: AE, env: Env): Num = expr match {
      case num @ Num(_) => num

      case Add(lhs, rhs) =>
        val lv = interp(lhs, env)
        val rv = interp(rhs, env)
        (lv, rv) match {
          case (Num(m), Num(n)) => Num(m + n)
          case _                => sys.error(s"Can only add numbers, but got $lhs and $rhs")
        }

      case Sub(lhs, rhs) =>
        val lv = interp(lhs, env)
        val rv = interp(rhs, env)
        (lv, rv) match {
          case (Num(m), Num(n)) => Num(m + n)
          case _                => sys.error(s"Can only subtract numbers, but got $lhs and $rhs")
        }
    }
  }

  object Substitution extends Arithmetic {

    override type Value = Num | Fun
    override type Env   = Unit

    def subst(expr: FLAE, substId: String, value: FLAE): FLAE = expr match {
      case Num(_)                => expr
      case Add(lhs, rhs)         => Add(subst(lhs, substId, value), subst(rhs, substId, value))
      case Sub(lhs, rhs)         => Sub(subst(lhs, substId, value), subst(rhs, substId, value))
      case Id(name)              => if substId == name then value else expr
      case App(funExpr, argExpr) => App(subst(funExpr, substId, value), subst(argExpr, substId, value))

      case Let(boundId, namedExpr, boundBody) =>
        val substNamedExpr = subst(namedExpr, substId, value)
        if boundId == substId then
            Let(boundId, substNamedExpr, boundBody)
        else
            Let(boundId, substNamedExpr, subst(boundBody, substId, value))

      // Example:
      // Fun("x", Fun("x", x + 1)) 1 2
      // -> Fun("x", x + 1) 2
      // -> 3
      case Fun(param, body) =>
        if param == substId
        then
            Fun(param, body)
        else
            Fun(param, subst(body, substId, value))

    }

    /** Note that the language is dynamically typed, but now we have two different value types (numbers and functions).
      * When we receive an unexpected value we simply throw an error
      * – we could add a type system to ensure these cases will never happen.
      */
    override def interp(expr: FLAE, env: Env): Value = expr match {
      case other: AE => interpAE(other, env)

      case Let(boundId, namedExpr, boundBody) =>
        val body = subst(boundBody, boundId, namedExpr)
        interp(body, env)

      case Id(name) => sys.error("found unbound id " + name)

      case f @ Fun(_, _) => f

      // funExpr can be an App:
      //   App(App(Fun("x", "x"), Fun("y", "y")), 5)
      case App(funExpr, argExpr) => interp(funExpr, env) match {
          case Fun(param, body) => interp(subst(body, param, argExpr), env)
          case v1               => sys.error(s"Expected function value but got $v1")
        }

    }
  }

  object DynamicScoping extends Arithmetic {
    override type Value = Num | Fun
    override type Env   = Map[String, Value]

    def interp(expr: FLAE, env: Env): Value = expr match {
      case other: AE => interpAE(other, env)

      case Let(boundId, namedExpr, body) =>
        interp(body, env + (boundId -> interp(namedExpr, env)))

      case Id(name) => env.getOrElse(name, sys.error(s"unbound variable $name"))

      case App(funExpr, argExpr) => interp(funExpr, env) match {
          case Fun(param, body) =>
            interp(body, env + (param -> interp(argExpr, env)))
          case _ => sys.error("Can only handle function expressions")
        }

      case f @ Fun(_, _) => f
    }
  }

  object StaticScoping extends Arithmetic {

    // note that closures should NOT be expressions!
    // (we don’t want programmers to write closures themselves as part of a program expression!)
    case class VClosure(param: String, body: FLAE, env: Env)

    override type Value = Num | VClosure
    override type Env   = Map[String, Value]

    def interp(expr: FLAE, env: Env): Value = expr match {
      case other: AE => interpAE(other, env)

      case Let(boundId, namedExpr, boundBody) =>
        interp(boundBody, env + (boundId -> interp(namedExpr, env)))

      case Id(name) => env.getOrElse(name, sys.error(s"unbound variable $name"))

      case Fun(param, body) => VClosure(param, body, env)

      // funExpr can be an App:
      //   App(App(Fun("x", "x"), Fun("y", "y")), 5)
      case App(funExpr, argExpr) => interp(funExpr, env) match {
          case VClosure(param, body, funEnv) =>
            val argVal           = interp(argExpr, env)
            val extendedEnv: Env = funEnv + (param -> argVal)
            interp(body, extendedEnv)

          case v1 => sys.error(s"Expected function value but got $v1")
        }

    }
  }
}
