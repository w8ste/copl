package interpreters.differentiation.old

object Reverse {

  sealed trait FCRV

  case class Num(n: Double)                                 extends FCRV
  case class Add(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Mul(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Sub(lhs: FCRV, rhs: FCRV)                      extends FCRV
  case class Let(name: String, namedExpr: FCRV, body: FCRV) extends FCRV
  case class Id(name: String)                               extends FCRV
  case class Fun(param: String, body: FCRV)                 extends FCRV
  case class App(funExp: FCRV, arg: FCRV)                   extends FCRV
  case class Diff(funExp: FCRV, arg: FCRV)                  extends FCRV
  // TODO: case class Diff2(exp: FCRV, vars: Array[String])          extends FCRV

  type TapeId = Int
  case class TapeEntry(y: TapeId, d: Double, x: TapeId) // dy/dx = d
  type Tape = Array[TapeEntry]

  case class VClosure(param: String, body: FCRV, env: Env)
  case class Dual(x: Double, id: TapeId)

  type Value = VClosure | Dual
  type Env   = Map[String, Value]

  var currentId: TapeId = 0
  def freshId(): TapeId =
      currentId += 1
      currentId

  var tape: Tape = Array.empty

  def interp(expr: FCRV, env: Env): Value = expr match {
    case num @ Num(x)  => Dual(x, 0)
    case Add(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (Dual(x1, id1), Dual(x2, id2)) =>
          val id = freshId()
          tape :+= TapeEntry(id, 1, id1)
          tape :+= TapeEntry(id, 1, id2)
          Dual(x1 + x2, id)
        case _ => sys.error(s"Can only add numbers, but got $lhs and $rhs")
      }
    case Mul(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (Dual(x1, id1), Dual(x2, id2)) =>
          val id = freshId()
          tape :+= TapeEntry(id, x2, id1)
          tape :+= TapeEntry(id, x1, id2)
          Dual(x1 * x2, id)
        case _ => sys.error(s"Can only multiply numbers, but got $lhs and $rhs")
      }
    case Sub(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (Dual(x1, id1), Dual(x2, id2)) =>
          val id = freshId()
          tape :+= TapeEntry(id, 1, id1)
          tape :+= TapeEntry(id, -1, id2)
          Dual(x1 - x2, id)
        case _ => sys.error(s"Can only subtract numbers, but got $lhs and $rhs")
      }
    case Let(boundId, namedExpr, boundBody) =>
      interp(boundBody, env + (boundId -> interp(namedExpr, env)))
    case Id(name)              => env.getOrElse(name, sys.error(s"unbound variable $name"))
    case Fun(param, body)      => VClosure(param, body, env)
    case App(funExpr, argExpr) =>
      interp(funExpr, env) match {
        case VClosure(param, body, funEnv) =>
          val argVal           = interp(argExpr, env)
          val extendedEnv: Env = funEnv + (param -> argVal)
          interp(body, extendedEnv)
        case v1 => sys.error(s"Expected function value but got $v1")
      }
    case Diff(funExpr, argExpr) =>
      interp(funExpr, env) match {
        case VClosure(param, body, funEnv) =>
          interp(argExpr, env) match {
            case Dual(x, _) =>
              tape = Array.empty
              val id1         = freshId()
              val extendedEnv = funEnv + (param -> Dual(x, id1))
              interp(body, extendedEnv) match {
                case Dual(_, id2) =>
                  var grads = Map(id2 -> 1.0)
                  for TapeEntry(y, d, x) <- tape.reverse do
                      grads = grads + (x -> (grads.getOrElse(x, 0.0) + grads.getOrElse(y, 0.0) * d))
                  Dual(grads.getOrElse(id1, 0), 0)
                case v3 => sys.error(s"Expected function to return number bot got $v3")
              }
            case v2 => sys.error(s"Expected number but got $v2")
          }
        case v1 => sys.error(s"Expected function value but got $v1")
      }
  }

  @main
  def RVtest(): Unit =
    println(interp(Diff(Fun("x", Mul(Id("x"), Mul(Id("x"), Id("x")))), Num(4)), Map.empty))
}
