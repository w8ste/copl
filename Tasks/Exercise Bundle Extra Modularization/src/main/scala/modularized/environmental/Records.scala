package modularized.environmental

import modularized.Expressions

trait Records extends BaseInterp, Expressions {

  case class RecordValue(fields: Map[String, Value])

  def interpRecords(expr: RecordExpr, env: Env): Value | RecordValue = expr match
      case Record(fields)                      => RecordValue(fields.map((label, expr) => (label, interp(expr, env))))
      case RecordProjection(recordExpr, label) =>
        interp(recordExpr, env) match
            case RecordValue(fields) => fields(label)
            case other               => sys.error(s"cannot lookup $label in $other")
}
