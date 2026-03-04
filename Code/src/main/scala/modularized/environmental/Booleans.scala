package modularized.environmental

import modularized.Expressions

trait Booleans extends BaseInterp, Expressions {

  def interpBooleans(expr: BooleanExpr, env: Env): Value | True.type | False.type = expr match {
    case True          => True
    case False         => False
    case And(lhs, rhs) => interp(lhs, env) match
          case False => False
          case True  => interp(rhs, env)
    case Or(lhs, rhs) => interp(lhs, env) match
          case True  => True
          case False => interp(rhs, env)
    case Not(e) => interp(e, env) match
          case True  => False
          case False => True

    // Note: this uses Scala’s built in equality on case classes (a form of meta interpretation).
    case Eq(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      if lv == rv then True else False

    case If(test, thenBody, elseBody) =>
      interp(test, env) match {
        case True  => interp(thenBody, env)
        case False => interp(elseBody, env)
        case other => sys.error(s"if requires a boolean value, but got $other")
      }
  }

}
