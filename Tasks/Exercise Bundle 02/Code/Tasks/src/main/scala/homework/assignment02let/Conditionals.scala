package homework.assignment02let

object Conditionals {
  sealed trait Expr
  case class Num(n: Int)                                     extends Expr
  case class Add(lhs: Expr, rhs: Expr)                       extends Expr
  case class Sub(lhs: Expr, rhs: Expr)                       extends Expr
  case class Let(name: String, namedExpr: Expr, body: Expr)  extends Expr
  case class Id(name: String)                                extends Expr
  case class If0(test: Expr, thenBody: Expr, elseBody: Expr) extends Expr

  import scala.language.implicitConversions
  implicit def symbolToExpr(symbol: String): Id = Id(symbol)
  implicit def intToExpr(n: Int): Num           = Num(n)

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  // evaluates F1LAE expressions by reducing them to their corresponding values
  def interp(expr: Expr): Int = expr match {
    case Num(n)                             => n
    case Add(lhs, rhs)                      => interp(lhs) + interp(rhs)
    case Sub(lhs, rhs)                      => interp(lhs) - interp(rhs)
    case Id(name)                           => sys.error("found unbound id " + name)
    case Let(boundId, namedExpr, boundExpr) =>
      interp(subst(boundExpr, boundId, Num(interp(namedExpr))))
    
    case If0(test, thenBody, elseBody) => {
      val t: Int = interp(test)
      if(t == 0)
        interp(thenBody);
      else 
        interp(elseBody);
    }

  }

  // substitutes 2nd argument with 3rd argument in 1st argument. The resulting
  // expression contains no free instances of the 2nd argument.
  def subst(expr: Expr, substId: String, value: Expr): Expr = expr match {
    case Num(n)                             => expr
    case Add(lhs, rhs)                      => Add(subst(lhs, substId, value), subst(rhs, substId, value))
    case Sub(lhs, rhs)                      => Sub(subst(lhs, substId, value), subst(rhs, substId, value))
    case Id(name)                           => if (substId == name) then value else expr
    case Let(boundId, namedExpr, boundExpr) =>
      val substNamedExpr = subst(namedExpr, substId, value)
      if boundId == substId then
        Let(boundId, substNamedExpr, boundExpr)
      else
        Let(boundId, substNamedExpr, subst(boundExpr, substId, value))

    
    case If0(test, thenBody, elseBody) => {
      If0(subst(test, substId, value), subst(thenBody, substId, value), subst(elseBody, substId, value))
    }

  }
}
