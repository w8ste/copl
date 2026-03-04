package homework.assignment03functions.part1

import scala.language.implicitConversions

object MultipleArguments {
  // AST for expressions of F1LAE
  sealed trait Expr
  case class Num(n: Int)                                    extends Expr
  case class Add(lhs: Expr, rhs: Expr)                      extends Expr
  case class Sub(lhs: Expr, rhs: Expr)                      extends Expr
  case class Let(name: String, namedExpr: Expr, body: Expr) extends Expr
  case class Id(name: String)                               extends Expr

  case class App(funName: String, arg: Expr) extends Expr

  // AST for function definition forms in Expr

  case class FunDef(funName: String, argName: String, body: Expr)

  // evaluates expressions by reducing them to their corresponding values
  def interp(expr: Expr, funDefs: Map[String, FunDef]): Int = expr match {
    case Num(n)                             => n
    case Add(lhs, rhs)                      => interp(lhs, funDefs) + interp(rhs, funDefs)
    case Sub(lhs, rhs)                      => interp(lhs, funDefs) - interp(rhs, funDefs)
    case Id(name)                           => sys.error("found unbound id " + name)
    case Let(boundId, namedExpr, boundExpr) =>
      interp(subst(boundExpr, boundId, Num(interp(namedExpr, funDefs))), funDefs)

    case App(funName, argExprs) => ???

  }

  // substitutes 2nd argument with 3rd argument in 1st argument. The resulting
  // expression contains no free instances of the 2nd argument.
  def subst(expr: Expr, substId: String, value: Expr): Expr = expr match {
    case Num(n)                             => expr
    case Add(lhs, rhs)                      => Add(subst(lhs, substId, value), subst(rhs, substId, value))
    case Sub(lhs, rhs)                      => Sub(subst(lhs, substId, value), subst(rhs, substId, value))
    case Id(name)                           => if substId == name then value else expr
    case Let(boundId, namedExpr, boundExpr) =>
      val substNamedExpr = subst(namedExpr, substId, value)
      if boundId == substId then
          Let(boundId, substNamedExpr, boundExpr)
      else
          Let(boundId, substNamedExpr, subst(boundExpr, substId, value))
    case App(funName, argExprs) => ???

  }
}
