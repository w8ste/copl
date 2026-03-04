package homework.assignment04recursionfix

object LetRecWithMultipleDefinitions {

  class Box[V](var value: V)

  sealed trait RecursiveFLAE
  type RCFLAE = RecursiveFLAE
  case class Num(n: Int)                                         extends RCFLAE
  case class Add(lhs: RCFLAE, rhs: RCFLAE)                       extends RCFLAE
  case class Sub(lhs: RCFLAE, rhs: RCFLAE)                       extends RCFLAE
  case class Mult(lhs: RCFLAE, rhs: RCFLAE)                      extends RCFLAE
  case class Let(name: String, namedExpr: RCFLAE, body: RCFLAE)  extends RCFLAE
  case class Id(name: String)                                    extends RCFLAE
  case class Fun(param: String, body: RCFLAE)                    extends RCFLAE
  case class If0(test: RCFLAE, posBody: RCFLAE, negBody: RCFLAE) extends RCFLAE

  case class LetRec(name: String, namedExpr: RCFLAE, body: RCFLAE) extends RCFLAE
  case class App(funExpr: RCFLAE, argExpr: RCFLAE)                 extends RCFLAE

  type Env = Map[String, Box[Value]]
  case class Closure(param: String, body: RCFLAE, env: Env)

  type Value = Num | Closure

  // Arithmetic expressions are exactly one of these cases
  type AE = Num | Sub | Add | Mult | If0

  def binOp(lhs: RCFLAE, rhs: RCFLAE, env: Env, combine: (Int, Int) => Int): Num =
    val lv = interp(lhs, env)
    val rv = interp(rhs, env)
    (lv, rv) match {
      case (Num(m), Num(n)) => Num(combine(m, n))
      case _                => sys.error(s"Can do arthmetic with numbers, but got $lhs and $rhs")
    }

  def interpAE(expr: AE, env: Env): Value | Num = {
    expr match {
      case num @ Num(_)                => num
      case Add(lhs, rhs)               => binOp(lhs, rhs, env, _ + _)
      case Sub(lhs, rhs)               => binOp(lhs, rhs, env, _ - _)
      case Mult(lhs, rhs)              => binOp(lhs, rhs, env, _ * _)
      case If0(test, posBody, negBody) =>
        if interp(test, env) == Num(0)
        then interp(posBody, env)
        else interp(negBody, env)
    }
  }

  def interp(expr: RCFLAE, env: Env = Map()): Value = expr match {

    case other: AE => interpAE(other, env)

    case Let(boundId, namedExpr, boundBody) =>
      interp(boundBody, env + (boundId -> Box(interp(namedExpr, env))))

    case Id(name) => env(name).value

    case Fun(arg, body) => Closure(arg, body, env)

    case LetRec(boundId, namedExpr, boundBody) =>
      val mutableBox = Box[Value](Num(-42))
      val recEnv     = env + (boundId -> mutableBox)
      val namedValue = interp(namedExpr, recEnv)
      mutableBox.value = namedValue
      interp(boundBody, recEnv)

    case App(funExpr, argExpr) =>
      val funV = interp(funExpr, env)
      funV match {
        case Closure(fParam, fBody, fEnv) =>
          interp(fBody, fEnv + (fParam -> Box(interp(argExpr, env))))
        case _ => sys.error("can only apply functions, but got: " + funV)
      }
  }
}
