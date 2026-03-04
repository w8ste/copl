package modularized

trait BaseTypeOf {

  case class Error(description: String)

  type Expr
  type Type >: Error

  type TEnv

  given typeEnvironment: (TEnv isEnvironmentOf Type)

  def typeOf(expr: Expr, env: TEnv): Type | Error

  def isSubtypeOf(actual: Type, expected: Type): Boolean

}

trait AllTypes extends RecordTypes, ArithmeticTypes, FunctionTypes, BooleanTypes, LetRecTypes

trait ArithmeticTypes extends BaseTypeOf, Expressions {
  // types

  case class TNum()

  def typeOfArithmetic(expr: ArithmeticExpr, env: TEnv): Type | TNum = expr match {

    case Num(_) => TNum()

    case Add(lhs, rhs)  => typeBinop(lhs, rhs, env)
    case Mult(lhs, rhs) => typeBinop(lhs, rhs, env)
    case Sub(lhs, rhs)  => typeBinop(lhs, rhs, env)

    case If0(c, ib, eb) if typeOf(c, env) == TNum() =>
      (typeOf(ib, env), typeOf(eb, env)) match {
        case (t1, t2) if t1 == t2 => t1
        case _                    => Error("Different types on branches")
      }
    case If0(c, _, _) => Error(s"type of $c is not a number")
  }

  private def typeBinop(lhs: Expr, rhs: Expr, ctx: TEnv): TNum | Error =
    if typeOf(lhs, ctx) == TNum() && typeOf(rhs, ctx) == TNum()
    then TNum()
    else Error("Arguments expected type TNum for arguments of Add")
}

trait BooleanTypes extends BaseTypeOf, Expressions {
  case object TBool
  def typeOfBooleans(expr: BooleanExpr, env: TEnv): Type | TBool.type =
      def check(expr: Expr) =
        typeOf(expr, env) match
            case TBool => TBool
            case other => Error(s"expected boolean type, but got $other for $expr")

      expr match
          case True                         => TBool
          case False                        => TBool
          case Not(expr)                    => check(expr)
          case And(lhs, rhs)                => check(lhs); check(rhs)
          case Or(lhs, rhs)                 => check(lhs); check(rhs)
          case Eq(lhs, rhs)                 => check(lhs); check(rhs)
          case If(test, thenBody, elseBody) =>
            check(test)
            val thenType = typeOf(thenBody, env)
            val elseType = typeOf(elseBody, env)
            if isSubtypeOf(thenType, elseType)
            then elseType
            else if isSubtypeOf(elseType, thenType)
            then thenType
            else Error(s"then and else branch have different types ($thenType != $elseType) in »$expr«")

}

trait FunctionTypes extends BaseTypeOf, Expressions {

  case class TFun(tparam: Type, treturn: Type)

  def typeOfFunctions(expr: FunExpr | TypedFun | Let, env: TEnv): Type | TFun = expr match {
    case App(funExpr, arg) =>
      typeOf(funExpr, env) match {
        case TFun(tParam, tReturn) =>
          val tArg = typeOf(arg, env)

          if isSubtypeOf(tArg, tParam)
          then tReturn
          else Error(s"Mismatching argument type. Expected $tParam but was $tArg")
        case t => Error(s"App expects function type as first argument, but was $t")
      }

    case _: Fun => Error("function require explicit ascription")

    case TypedFun(param, typeAnnotation, body) =>
      val newTEnv = env.updated(param, typeAnnotation)
      TFun(typeAnnotation, typeOf(body, newTEnv))

    case Id(x) => env.lookup(x)

    case Let(name, namedExpr, body) =>
      val namedExprT = typeOf(namedExpr, env)
      val newTEnv    = env.updated(name, namedExprT)
      typeOf(body, newTEnv)
  }
}

trait RecordTypes extends BaseTypeOf, Expressions {

  case class TRecord(fields: Map[String, Type])

  def typeOfRecords(expr: RecordExpr, env: TEnv): Type | TRecord = expr match
      case Record(fieldList) =>
        val typedfields = fieldList.map(t => (t._1, typeOf(t._2, env)))
        TRecord(typedfields)

      case RecordProjection(rec, label) => typeOf(rec, env) match {
          case TRecord(typedfields) => typedfields(label)
          case _                    => Error(s"Projection on an expression that is not a record: $rec")
        }
}

trait LetRecTypes extends BaseTypeOf, Expressions {
  def typeOfRecursion(expr: Expr, env: TEnv): Type = expr match
      case LetRec(boundId, namedExpr, boundBody) =>
        val namedType = typeOf(namedExpr, env)
        typeOf(boundBody, env.updated(boundId, namedType))
}
