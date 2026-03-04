package interpreters.lambdacalculus

object LambdaCalculus {

  sealed trait Expression
  sealed trait Value

  case class Id(name: String)                         extends Expression
  case class Fun(param: String, body: Expression)     extends Expression with Value
  case class App(funExp: Expression, arg: Expression) extends Expression

  def interp(expr: Expression): Value = expr match {

    case Id(name) => sys.error(s"found unbound id $name")

    case f @ Fun(_, _) => f

    // Call-by-name semantics: argExpr is not evaluated (interpreted) before substitution.
    case App(funExpr, argExpr) => interp(funExpr) match {
        case Fun(param, body) => interp(subst(body, param, argExpr))
      }
  }

  ///////////////////////////////////////////////
  /* everything below is just for substitution */
  ///////////////////////////////////////////////

  def freeVariables(expr: Expression): Set[String] = expr match {
    case Id(x)            => Set(x)
    case Fun(param, body) => freeVariables(body) - param
    case App(e1, e2)      => freeVariables(e1) ++ freeVariables(e2)
  }

  def subst(expr: Expression, substId: String, value: Expression): Expression = expr match {
    case Id(name) =>
      if substId == name then value
      else expr

    case Fun(param, body) =>
      if param == substId
      then Fun(param, body)
      else if !freeVariables(value).contains(param)
      then
          Fun(param, subst(body, substId, value))
      else
          val potentialConflicts = freeVariables(value) union freeVariables(body)
          val freshId            = Id(freshName(param, potentialConflicts))
          val alphaRenamed       = subst(body, param, freshId)
          Fun(freshId.name, subst(alphaRenamed, substId, value))

    case App(funExpr, argExpr) =>
      App(subst(funExpr, substId, value), subst(argExpr, substId, value))
  }

  def freshName(id: String, known: Set[String]): String =
      def rec(count: Int): String =
          val newName = s"$id-$count"
          if known.contains(newName)
          then rec(count + 1)
          else newName
      rec(0)

}
