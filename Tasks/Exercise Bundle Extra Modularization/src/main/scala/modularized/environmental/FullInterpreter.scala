package modularized.environmental

import modularized.{ArithmeticTypes, BooleanTypes, FunctionTypes, RecordTypes}

object FullInterpreter extends Recursion, Arithmetic, Functions, Booleans, Records, AlgebraicData, ArithmeticTypes,
      BooleanTypes, RecordTypes, FunctionTypes, MostTypesImpl {

  case object TUnspecified

  override type Expr =
    ArithmeticExpr | FunExpr | TypedFun | BooleanExpr | LetRec | Let | RecordExpr | AlgebraicDataExpr | Sym
  override type Value = Num | Closure | True.type | False.type | RecordValue | Constructor | Sym
  override type Env   = FullEnvironment
  override type Type  = TNum | TFun | TBool.type | TRecord | TUnspecified.type | Error

  def emptyEnvironment: FullEnvironment = FullEnvironment(Map.empty)

  def interp(expr: Expr): Value = interp(expr, emptyEnvironment)

  override def interp(expr: Expr, env: Env): Value = expr match
      case e: ArithmeticExpr    => interpArithmetic(e, env)
      case e: BooleanExpr       => interpBooleans(e, env)
      case TypedFun(p, _, b)    => interpFunctions(Fun(p, b), env)
      case e: (FunExpr | Let)   => interpFunctions(e, env)
      case e: LetRec            => interpRecursion(e, env)
      case e: RecordExpr        => interpRecords(e, env)
      case e: AlgebraicDataExpr => interpAlgebraicData(e, env)
      case e: Sym               => e

  /** “Lazy values” are not first class values in Scala.
    * This helper class combines by name parameters and a lazy val to create a first class lazy value.
    * (First class, in this case, means that we can store it in the dictionary below.)
    */
  class Lazy[V](x: => V) {
    lazy val value: V = x
  }
  case class FullEnvironment(dict: Map[String, Lazy[Value]])

  override given valueEnvironment: RecursiveEnvironment[FullEnvironment, Value] with {
    extension (env: FullEnvironment)
        override def bindRecursive(id: String, value: => Value | Null): FullEnvironment =
            val newValue: Lazy[Value] = Lazy:
                // Option(value) does return None when value is null
                Option(value).getOrElse(sys.error(s"tried to access $id, but binding was not yet evaluated")).nn
            FullEnvironment(env.dict.updated(id, newValue))
    extension (env: FullEnvironment)
        override def lookup(id: String): Value =
          env.dict.getOrElse(id, sys.error(s"not in scope: $id")).value
  }

}
