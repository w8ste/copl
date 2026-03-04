package interpreters.conditionals

object ConditionalsDynamicFirstClass {
  sealed trait ConditionalFLAE
  type CFLAE = ConditionalFLAE
  case class Num(n: Int)                                       extends CFLAE
  case class Add(lhs: CFLAE, rhs: CFLAE)                       extends CFLAE
  case class Sub(lhs: CFLAE, rhs: CFLAE)                       extends CFLAE
  case class Mult(lhs: CFLAE, rhs: CFLAE)                      extends CFLAE
  case class Let(name: String, namedCFLAE: CFLAE, body: CFLAE) extends CFLAE
  case class Id(name: String)                                  extends CFLAE
  case class If0(test: CFLAE, posBody: CFLAE, negBody: CFLAE)  extends CFLAE
  case class App(funExpr: CFLAE, arg: CFLAE)                   extends CFLAE
  case class Fun(param: String, body: CFLAE)                   extends CFLAE

  type Value = Num | Fun
  type Env   = Map[String, Value]

  def interp(expr: CFLAE, env: Env = Map()): Value = expr match {
    case num @ Num(_) => num

    case Add(lhs, rhs) => (interp(lhs, env), interp(rhs, env)) match {
        case (Num(m), Num(n)) => Num(m + n)
        case _                => sys.error(s"Can only add numbers, but got $lhs and $rhs")
      }

    case Sub(lhs, rhs) => (interp(lhs, env), interp(rhs, env)) match {
        case (Num(m), Num(n)) => Num(m - n)
        case _                => sys.error(s"Can only subtract numbers, but got $lhs and $rhs")
      }

    case Mult(lhs, rhs) => (interp(lhs, env), interp(rhs, env)) match {
        case (Num(m), Num(n)) => Num(m * n)
        case _                => sys.error(s"Can only multiply numbers, but got $lhs and $rhs")
      }

    case Let(boundId, namedExpr, body) =>
      interp(body, env + (boundId -> interp(namedExpr, env)))

    case Id(name) => env(name)

    case If0(testExpr, thenExpr, elseExpr) =>
      val testV = interp(testExpr, env)
      testV match {
        case Num(0) => interp(thenExpr, env)
        case Num(_) => interp(elseExpr, env)
        case _      => sys.error(s"Can only test numbers, but got $testV")
      }

    case App(funExpr, argExpr) => interp(funExpr, env) match {
        case Fun(param, body) =>
          interp(body, env + (param -> interp(argExpr, env)))

        case _ => sys.error(s"Can only apply functions, but got $funExpr")
      }
    case f @ Fun(_, _) => f
  }
}
