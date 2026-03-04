package homework.assignment12types

import interpreters.types.{BaseExpression, RecordExpressions}

object TypesForState {

  type Location = Int

  trait StateExpression extends BaseExpression {

    sealed trait StateExpr
    case class Seqn(e1: Expr, e2: Expr)               extends StateExpr
    case class SetId(id: String, valueExpr: Expr)     extends StateExpr
    case class NewBox(valExpr: Expr)                  extends StateExpr
    case class SetBox(boxExpr: Expr, valueExpr: Expr) extends StateExpr
    case class OpenBox(boxExpr: Expr)                 extends StateExpr

    case class TBox(boxtype: Type)

    def typeOfState(e: StateExpr, ctx: Ctx): Type | TBox = e match {

      case Seqn(e1, e2) => ???

      case NewBox(valExpr) => ???

      case SetBox(boxExpr, valueExpr) => ???

      case OpenBox(boxExpr) => ???

      case SetId(id, valueExpr) => ???

    }

  }

  object StateWithRecords extends RecordExpressions with StateExpression {
    override type Expr = RecordExpr | StateExpr | BaseExpr
    override type Type = TRecord | TBox | BaseType
    override def typeOf(e: Expr, ctx: Ctx): Type = e match
        case e: BaseExpr   => typeOfBase(e, ctx)
        case e: RecordExpr => typeOfRecord(e, ctx)
        case e: StateExpr  => typeOfState(e, ctx)
  }

}
