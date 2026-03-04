package modularized.environmental

import modularized.{AllTypes, Expressions, isEnvironmentOf}

trait MostTypesImpl extends AllTypes with Expressions {

  override type Type >: (TNum | TBool.type | TFun | TRecord | Error)

  override type TEnv = Map[String, Type]

  override def typeOf(expr: Expr, env: TEnv): Type = expr match
      case e: ArithmeticExpr             => typeOfArithmetic(e, env)
      case e: BooleanExpr                => typeOfBooleans(e, env)
      case e: (FunExpr | Let | TypedFun) => typeOfFunctions(e, env)
      case e: LetRec                     => typeOfRecursion(e, env)
      case e: RecordExpr                 => typeOfRecords(e, env)
      case e: (AlgebraicDataExpr | Sym)  => sys.error("no typechecker yet!")

  override given typeEnvironment: (TEnv isEnvironmentOf Type) with {
    extension (env: TEnv)
        def lookup(id: String): Type               = env(id)
        def updated(id: String, value: Type): TEnv = env.updated(id, value)
  }

  override def isSubtypeOf(actual: Type, expected: Type): Boolean = (actual, expected) match {
    case (a, b) if a == b => true

    case (TRecord(tl1), TRecord(tl2)) =>
      val tl1m = tl1
      tl2.forall(p =>
        tl1m.get(p._1) match {
          case Some(t) => isSubtypeOf(t, p._2)
          case None    => false
        }
      )

    case (TFun(ta1, tb1), TFun(ta2, tb2)) => isSubtypeOf(ta2, ta1) && isSubtypeOf(tb1, tb2)

    case _ => false
  }
}
