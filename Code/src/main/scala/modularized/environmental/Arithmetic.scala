package modularized.environmental

import modularized.Expressions

trait Arithmetic extends BaseInterp, Expressions {

  private def interpBinop(lhs: Expr, rhs: Expr, op: (Int, Int) => Int, env: Env): Num =
    (interp(lhs, env), interp(rhs, env)) match {
      case (Num(n1), Num(n2)) => Num(op(n1, n2))
      case v                  => sys.error(s"Expected numeric values but got $v")
    }

  def interpArithmetic(e: ArithmeticExpr, env: Env): Value | Num = e match {

    case Num(n) => Num(n)

    case Add(lhs, rhs) => interpBinop(lhs, rhs, _ + _, env)

    case Sub(lhs, rhs) => interpBinop(lhs, rhs, _ - _, env)

    case Mult(lhs, rhs) => interpBinop(lhs, rhs, _ * _, env)

    case If0(test, thenBody, elseBody) =>
      interp(test, env) match {
        case Num(0) => interp(thenBody, env)
        case Num(_) => interp(elseBody, env)
        case other  => sys.error(s"If0 requires a number but got: $other")
      }

  }

}
