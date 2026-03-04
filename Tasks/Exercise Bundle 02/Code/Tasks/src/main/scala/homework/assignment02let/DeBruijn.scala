package homework.assignment02let

object DeBruijn {
  sealed trait Expr
  case class NumDB(n: Int)                      extends Expr
  case class AddDB(lhs: Expr, rhs: Expr)        extends Expr
  case class SubDB(lhs: Expr, rhs: Expr)        extends Expr
  case class LetDB(namedExpr: Expr, body: Expr) extends Expr
  case class RefDB(n: Int)                      extends Expr

  sealed trait LAE
  case class Num(n: Int)                                     extends LAE
  case class Add(lhs: LAE, rhs: LAE)                         extends LAE
  case class Sub(lhs: LAE, rhs: LAE)                         extends LAE
  case class Let(boundId: String, namedExpr: LAE, body: LAE) extends LAE
  case class Id(name: String)                                extends LAE

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  def convert(expr: LAE, subs: List[String] = List()): Expr = expr match {
    case Num(n)        => NumDB(n)
    case Add(lhs, rhs) => AddDB(convert(lhs, subs), convert(rhs, subs))
    case Sub(lhs, rhs) => SubDB(convert(lhs, subs), convert(rhs, subs))
    
      case Let(boundId, namedExpr, body) => ???
      case Id(name) => ??? 
  }

  def interp(expr: Expr, subs: List[Int] = List()): Int = expr match {
    case NumDB(n)        => n
    case AddDB(lhs, rhs) => interp(lhs, subs) + interp(rhs, subs)
    case SubDB(lhs, rhs) => interp(lhs, subs) - interp(rhs, subs)
    
    case LetDB(namedExpr, body) => ???
    case RefDB(n)               => ??? 

  }
}
