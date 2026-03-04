package interpreters.basic

object LetArithmeticExpressions {

  sealed trait Expr
  case class Num(n: Int)                                    extends Expr
  case class Add(lhs: Expr, rhs: Expr)                      extends Expr
  case class Sub(lhs: Expr, rhs: Expr)                      extends Expr
  case class Let(name: String, namedExpr: Expr, body: Expr) extends Expr
  case class Id(name: String)                               extends Expr

  type Value = Int

  def interp(expr: Expr, eager: Boolean): Value = expr match {
    case Num(n)        => n
    case Add(lhs, rhs) => interp(lhs, eager) + interp(rhs, eager)
    case Sub(lhs, rhs) => interp(lhs, eager) - interp(rhs, eager)
    case Id(name)      => sys.error(s"found unbound id $name")

    case Let(boundId, namedExpr, boundExpr) =>
      // the one difference between lazy and eager substitution is
      // weather we evaluate the `namedExpr` before substituting or not
      val valueToBeSubstituted =
        if eager
        then Num(interpEager(namedExpr))
        else namedExpr
      interpLazy(subst(boundExpr, boundId, valueToBeSubstituted))

  }

  def interpEager(expr: Expr): Value = interp(expr, eager = true)
  def interpLazy(expr: Expr): Value  = interp(expr, eager = false)

  /** Substitutes [[substId]] with [[value]] in [[expr]].
    * Warning, this implementation of substitution potentially causes free variables to be incorrectly captured.
    * See exercise 02 for a discussion of these issues.
    */
  def subst(expr: Expr, substId: String, value: Expr): Expr = expr match {
    case Num(_) => expr

    case Add(lhs, rhs) =>
      Add(subst(lhs, substId, value), subst(rhs, substId, value))

    case Sub(lhs, rhs) =>
      Sub(subst(lhs, substId, value), subst(rhs, substId, value))

    case Let(boundId, namedExpr, boundExpr) =>
      val substNamedExpr = subst(namedExpr, substId, value)
      if boundId == substId then
        Let(boundId, substNamedExpr, boundExpr)
      else
        Let(boundId, substNamedExpr, subst(boundExpr, substId, value))

    case Id(name) =>
      if substId == name then value else expr

  }
}
