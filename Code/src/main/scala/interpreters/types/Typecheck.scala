package interpreters.types

trait BaseExpression {

  // We abstract over the type of expression, so we can extend it in subclasses
  type Expr

  sealed trait BaseExpr
  case class Num(n: Int)                                    extends BaseExpr
  case class Add(lhs: Expr, rhs: Expr)                      extends BaseExpr
  case class App(funExpr: Expr, arg: Expr)                  extends BaseExpr
  case class Id(id: String)                                 extends BaseExpr
  case class Let(name: String, namedExpr: Expr, body: Expr) extends BaseExpr
  case class If0(test: Expr, posBody: Expr, negBody: Expr)  extends BaseExpr
  // We require users to annotate the type of function parameters.
  case class Fun(param: String, typ: Type, body: Expr) extends BaseExpr

  // similar to expressions, we abstract over the type of types
  type Type

  sealed trait BaseType
  case class TNum()                            extends BaseType
  case class TFun(tparam: Type, treturn: Type) extends BaseType

  // To keep track of variable bindings, we need to simulate the environment
  // of our interpreter in the type checker. The environment maps variable
  // names to their value at run time:
  // type Val = …
  // type Env = Map[String, Val]

  // At compile time, we don’t know the actual values. But we can assert what
  // their types are. That is, for each variable, we keep track of the type of
  // values that the variable can refer to:
  type Ctx = Map[String, Type]
  // This structure is called a typing context. Since we want to prevent errors
  // that might occur at run time during interpretation of the program, our type
  // checker needs to simulate the actions performed by the interpreter. That is,
  // whenever the interpreter changes the environment, our type checker must
  // perform a similar change to the typing context.

  // The full typeOf function handling any expression is defined in a subclass
  def typeOf(e: Expr, ctx: Ctx): Type

  def typeOfBase(e: BaseExpr, ctx: Ctx = Map()): Type | BaseType = e match {

    case Num(_) => TNum()

    case Add(lhs, rhs) if typeOf(lhs, ctx) == TNum() && typeOf(rhs, ctx) == TNum() => TNum()
    case Add(_, _) => sys.error("Arguments expected type TNum for arguments of Add")

    case App(funExpr, arg) =>
      typeOf(funExpr, ctx) match {
        case TFun(tParam, tReturn) =>
          val tArg = typeOf(arg, ctx)

          if typesMatch(tArg, tParam)
          then tReturn
          else sys.error(s"Mismatching argument type. Expected $tParam but was $tArg")
        case t => sys.error(s"App expects function type as first argument, but was $t")
      }

    // Q: what should we use as argument type?
    // A: require type annotation by the user
    case Fun(param, typeAnnotation, body) =>
      val ctx1 = ctx + (param -> typeAnnotation)
      TFun(typeAnnotation, typeOf(body, ctx1))

    // Q: what is the type of a variable reference? How to detect unbound variables?
    // A: need to keep track of bindings by simulating environment statically.
    case Id(x) => ctx.getOrElse(x, sys.error(s"Unbound variable $x"))

    case Let(name, namedExpr, body) =>
      val namedExprT = typeOf(namedExpr, ctx)
      val ctx1       = ctx + (name -> namedExprT)
      typeOf(body, ctx1)

    case If0(c, ib, eb) if typeOf(c, ctx) == TNum() =>
      (typeOf(ib, ctx), typeOf(eb, ctx)) match {
        case (t1, t2) if t1 == t2 => t1
        case _                    => sys.error("Different types on branches")
      }
    case If0(c, _, _) => sys.error(s"type of $c is not a number")
  }

  def typesMatch(actual: Type, expected: Type): Boolean =
    actual == expected

}

object Base extends BaseExpression {

  override type Expr = BaseExpr
  override type Type = BaseType

  override def typeOf(e: Expr, ctx: Base.Ctx): Base.Type = typeOfBase(e, ctx)
}

trait RecordExpressions extends BaseExpression {

  sealed trait RecordExpr
  case class Record(fieldList: List[(String, Expr)])    extends RecordExpr
  case class RecordProjection(rec: Expr, label: String) extends RecordExpr

  case class TRecord(tfieldList: List[(String, Type)])

  def typeOfRecord(e: RecordExpr, ctx: Ctx): Type | TRecord = e match
      case Record(fieldList) =>
        val typedfields: List[(String, Type)] = fieldList.map(t => (t._1, typeOf(t._2, ctx)))
        TRecord(typedfields)

      case RecordProjection(rec, label) => typeOf(rec, ctx) match {
          case TRecord(typedfields) => typedfields.toMap.apply(label)
          case _                    => sys.error("Projection on an expression that is not a record: " + rec)
        }
}

object Records extends RecordExpressions {
  override type Type = BaseType | TRecord
  override type Expr = BaseExpr | RecordExpr

  @main
  def example: Unit =
      val record   = Record(List("a" -> Num(4), "b" -> Num(5)))
      val function = Fun("x", TRecord(List("a" -> TNum())), RecordProjection(Id("x"), "a"))
      println(typeOf(record, Map.empty))
      println:
          typeOf(function, Map.empty)

      println(typeOf(App(function, record), Map.empty))

  override def typeOf(e: Expr, ctx: Ctx): BaseType | TRecord =
    e match
        case e: RecordExpr => typeOfRecord(e, ctx)
        case e: BaseExpr   => typeOfBase(e, ctx)
}

object Subtyping extends RecordExpressions {

  override def typesMatch(actual: Type, expected: Type): Boolean = isSubtypeOf(actual, expected)

  def isSubtypeOf(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (a, b) if a == b => true

    case (TRecord(tl1), TRecord(tl2)) =>
      val tl1m = tl1.toMap
      tl2.forall(p =>
        tl1m.get(p._1) match {
          case Some(t) => isSubtypeOf(t, p._2)
          case None    => false
        }
      )

    case (TFun(ta1, tb1), TFun(ta2, tb2)) => isSubtypeOf(ta2, ta1) && isSubtypeOf(tb1, tb2)

    case _ => false
  }

  override type Expr = RecordExpr | BaseExpr
  override type Type = TRecord | BaseType

  override def typeOf(e: Expr, ctx: Ctx): BaseType | TRecord =
    e match
        case e: RecordExpr => typeOfRecord(e, ctx)
        case e: BaseExpr   => typeOfBase(e, ctx)

}
