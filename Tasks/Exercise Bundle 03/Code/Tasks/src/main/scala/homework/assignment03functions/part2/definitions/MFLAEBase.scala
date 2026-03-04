package homework.assignment03functions.part2.definitions

object MFLAEBase {

  /** MFLAE: multiple-argument functions, lets and arithmetic expressions */
  sealed trait MFLAE
  case class MLNum(n: Int)                                      extends MFLAE
  case class MLAdd(lhs: MFLAE, rhs: MFLAE)                      extends MFLAE
  case class MLSub(lhs: MFLAE, rhs: MFLAE)                      extends MFLAE
  case class MLLet(name: String, namedExpr: MFLAE, body: MFLAE) extends MFLAE
  case class MLId(name: String)                                 extends MFLAE
  case class MLFun(params: List[String], body: MFLAE)           extends MFLAE
  case class MLApp(funExpr: MFLAE, args: List[MFLAE])           extends MFLAE

}
