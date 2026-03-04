package modularized.environmental

import modularized.Expressions

trait AlgebraicData extends BaseInterp with Expressions {

  case class Constructor(name: String, args: List[Value])

  def interpAlgebraicData(expr: AlgebraicDataExpr, env: Env): Value | Constructor = expr match
      case Data(name, args) => Constructor(name, args.map(arg => interp(arg, env)))

      case Match(expr, cases) => interp(expr, env) match {
          case value @ Constructor(name, argValues) =>
            val interpretedValue: Option[Value] = cases.collectFirst {
              case (caseName, caseStrings, caseExpr) if caseName == name && argValues.length == caseStrings.length =>
                val newEnv = (caseStrings zip argValues).foldLeft(env): (env, tuple) =>
                    env.updated.tupled(tuple)
                interp(caseExpr, newEnv)
            }

            interpretedValue match {
              case None =>
                sys.error(s"\nWanted to match: ${value.name}(${value.args.mkString(", ")}), options were:\n\t${
                    cases.map {
                      case (name, args, _) => s"$name(${args.mkString(", ")})"
                    }.mkString("\n\t")
                  }")
              case Some(v) => v
            }

          case v => sys.error(s"Expected data value but got $v")
        }

}
