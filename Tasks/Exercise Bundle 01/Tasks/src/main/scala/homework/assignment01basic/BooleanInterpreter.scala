package homework.assignment01basic

object BooleanInterpreter {

  /** Boolean expressions represents the abstract syntax of our language,
    * i.e. the representation a parser would generate by reading the program code.
    * This will be consumed by the interpreter (the eval function) to produce a result Value.
    */
  sealed trait Expr
  case object True                     extends Expr
  case object False                    extends Expr
  case class Not(expr: Expr)           extends Expr
  case class And(lhs: Expr, rhs: Expr) extends Expr
  case class Or(lhs: Expr, rhs: Expr)  extends Expr

  /** BooleanValue represents the result of our interpreter, it is conceptually identical to Scalas build in Boolean type,
    * but we we wanted to be very explicit here, that an interpreter will generally always take expressions which you defined,
    * and return values that you also defined. Neither have to be built in types.
    */
  sealed trait Value
  case object TrueValue  extends Value
  case object FalseValue extends Value

  // only modify this function
  def interp(expr: Expr): Value = expr match {
    case True => TrueValue
    case False => FalseValue
    case And(lhs, rhs) =>
      interp(lhs) match {
        case FalseValue => FalseValue
        case TrueValue  => interp(rhs)
      }

    // --------------------------------------------
    // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
    // --------------------------------------------

    
    case Not(expr) =>  {
      interp(expr) match {
        case FalseValue => TrueValue
        case TrueValue => FalseValue
      }
    }
    case Or(lhs, rhs) => 
      interp(lhs) match {
        case FalseValue => interp(rhs)
        case TrueValue => TrueValue
      }
  }

}
