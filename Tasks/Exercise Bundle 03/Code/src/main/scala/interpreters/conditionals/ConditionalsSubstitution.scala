package interpreters.conditionals

import modularized.Expressions
import modularized.substitution.StandardSubstitution

object ConditionalsSubstitution {

  /* In case you come across this code as part of the homwork and wonder what is going on:
   * We use a single general substitution implementation that can substitute any expression that we have in the lecture.
   * This substitution function is defined in StandardSubstitution.
   *
   * However, we want the substitution to be both extensible, and restrictable:
   * We want to be able to add new cases to substitute, but we also want to restrict it to only work with the language we are currently using (such as F1LAE below).
   * To achieve this, we use the abstract type Expr, which we define below to only include the cases we want to support.
   * This causes the subst function below to only work with expressions of that type.  */

  object F1LAE extends Expressions with StandardSubstitution {
    override type Expr = Num | Add | Sub | Let | Id | AppFirstOrder | If0
  }
  /* export makes thes functions available when we `import ConditionalsSubstitution.*`  */
  export F1LAE.{Add, AppFirstOrder as App, Expr, Id, If0, Let, Num, Sub, subst, intLiteralToNumExpr, stringLiteralToIdExpr}

  case class FunDef(argName: String, body: Expr)

  def interp(expr: Expr, funDefs: Map[String, FunDef]): Int = expr match {
    case Num(n)                             => n
    case Add(lhs, rhs)                      => interp(lhs, funDefs) + interp(rhs, funDefs)
    case Sub(lhs, rhs)                      => interp(lhs, funDefs) - interp(rhs, funDefs)
    case Id(name)                           => sys.error("found unbound id " + name)
    case Let(boundId, namedExpr, boundExpr) =>
      interp(subst(boundExpr, boundId, Num(interp(namedExpr, funDefs))), funDefs)
    case App(funName, argExpr) => funDefs(funName) match {
        case FunDef(argName, body) =>
          interp(subst(body, argName, interp(argExpr, funDefs)), funDefs)
      }
    case If0(test, thenBody, elseBody) =>
      if interp(test, funDefs) == 0 then
        interp(thenBody, funDefs)
      else
        interp(elseBody, funDefs)
  }

}
