package homework.assignment01basic

object Preprocessor {

  /** Boolean expressions represents the abstract syntax of our language,
    * i.e. the representation a parser would generate by reading the program code.
    * This will be consumed by the interpreter (the eval function) to produce a result Value.
    */
  sealed trait Expr
  case object True                       extends Expr
  case object False                      extends Expr
  case class Not(expr: Expr)             extends Expr
  case class And(lhs: Expr, rhs: Expr)   extends Expr
  case class Or(lhs: Expr, rhs: Expr)    extends Expr
  case class Imp(lhs: Expr, rhs: Expr)   extends Expr
  case class BiImp(lhs: Expr, rhs: Expr) extends Expr

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  // implement this function
  def preproc(expr: Expr): Expr = ???

}
