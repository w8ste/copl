package modularized

// Enables the implicit def feature below.
// This is considered a potentially confusing feature, thus the import to prevent accidental use.
// However, the way we use it (to convert literals) is generally considered safe,
// and there is a proposal to allow our use directly, but it is not yet available in stable scala:
// https://docs.scala-lang.org/scala3/reference/experimental/numeric-literals.html
import scala.language.implicitConversions

trait Expressions {

  /** All expressions below make use of this type alias to refer to their subexpressions,
    * thus, by overriding it, you can pick and choose (or even add) the allowed expressions in the language you want to define.
    */
  type Expr

  /** Includes a type because a naive type checker requires function type annotations */
  type Type

  // the following two definitions add implicit conversions, these allow us to write a literal Int or String,
  // and have that literal automatically converted
  implicit def intLiteralToNumExpr(n: Int): Num          = Num(n)
  implicit def stringLiteralToIdExpr(symbol: String): Id = Id(symbol)

  //////////////////////////////////
  // basic expressions
  //////////////////////////////////

  case class Sym(name: String)

  sealed trait ArithmeticExpr
  case class Num(n: Int)                                     extends ArithmeticExpr
  case class Add(lhs: Expr, rhs: Expr)                       extends ArithmeticExpr
  case class Sub(lhs: Expr, rhs: Expr)                       extends ArithmeticExpr
  case class Mult(lhs: Expr, rhs: Expr)                      extends ArithmeticExpr
  case class If0(test: Expr, thenBody: Expr, elseBody: Expr) extends ArithmeticExpr

  sealed trait BooleanExpr
  case object True                                          extends BooleanExpr
  case object False                                         extends BooleanExpr
  case class Not(expr: Expr)                                extends BooleanExpr
  case class And(lhs: Expr, rhs: Expr)                      extends BooleanExpr
  case class Or(lhs: Expr, rhs: Expr)                       extends BooleanExpr
  case class Eq(lhs: Expr, rhs: Expr)                       extends BooleanExpr
  case class If(test: Expr, thenBody: Expr, elseBody: Expr) extends BooleanExpr

  //////////////////////////////////
  // The λ calculus!
  //////////////////////////////////

  sealed trait FunExpr
  case class Id(name: String)               extends FunExpr
  case class Fun(param: String, body: Expr) extends FunExpr
  case class App(funExp: Expr, arg: Expr)   extends FunExpr

  case class AppFirstOrder(funName: String, arg: Expr)

  //////////////////////////////////
  // “Extensions” to the λ calculus
  //////////////////////////////////

  // typed function
  case class TypedFun(param: String, paramType: Type, body: Expr)

  case class Let(name: String, namedExpr: Expr, body: Expr)

  case class LetRec(name: String, namedExpr: Expr, body: Expr)

  //////////////////////////////////
  // Mutability
  //////////////////////////////////

  /* the * signifies that this is a “Varargs” argument, so you can call it with any number of arguments, for example:
   * ```
   *  Seq(e1, e2, e3)
   * ```
   */
  case class Seqn(exprs: Expr*) {
    // This is a runtime check that is performed when the value is constructed.
    require(exprs.nonEmpty, "Seqn may not be empty")
  }

  case class SetId(id: String, valueExpr: Expr)

  sealed trait BoxExpr
  case class NewBox(valExpr: Expr)                  extends BoxExpr
  case class SetBox(boxExpr: Expr, valueExpr: Expr) extends BoxExpr
  case class OpenBox(boxExpr: Expr)                 extends BoxExpr

  //////////////////////////////////
  // Data structures
  //////////////////////////////////

  sealed trait RecordExpr
  case class Record(fields: Map[String, Expr])          extends RecordExpr
  case class RecordProjection(rec: Expr, label: String) extends RecordExpr

  sealed trait AlgebraicDataExpr
  case class Data(name: String, args: List[Expr] = Nil) extends AlgebraicDataExpr

  /** matches expression, each element of the list destructures one data constructor with the same name and executes the expression as its body */
  case class Match(expr: Expr, cases: List[(String, List[String], Expr)]) extends AlgebraicDataExpr

  //////////////////////////////////
  // Continuations
  //////////////////////////////////

  sealed trait ContinuationExpr
  case class BindCC(name: String, body: Expr) extends ContinuationExpr
  case class AppK(kExpr: Expr, arg: Expr)     extends ContinuationExpr

  /** Convenience definition to access all expressions that are known at the top level.
    * You can still add new expressions to `Expr`, but these here are likely to be handled by existing convenience definitions
    */
  type KnownExpressions =
    ArithmeticExpr | BooleanExpr | FunExpr | TypedFun | Let | LetRec | Seqn | SetId | BoxExpr | RecordExpr | AlgebraicDataExpr | AppFirstOrder

  /** The intersection of the expressions a subclass selects, and the ones that are well known.
    * This is what the standard implementations will be able to handle
    */
  type StandardExpressions = KnownExpressions & Expr
}
