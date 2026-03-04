package modularized.stateful

import modularized.Expressions

trait Base {

  type Expr

  type Value

  type Env

  /** For the interpreters we consider here an expression does not only return a value, but also has some “effect”.
    * We want to remain generic with the effects we support, thus we just specify that the interpretation result is “wrapped” within some other type.
    * This Wrapper type is what tracks any effects that should happen (such as mutations of the store.
    *
    * However, to work with wrapped values, we need to be able to combine them sequentially:
    * For example, adding two values interprets the left hand side expression, then interprets the right hand side expression, and finally adds the two results.
    * This concept of “do one thing first, then do another (while keeping in mind whatever effects the first things had” is what we express with a this `given Pipeline[Wrapper]`.
    * For anything with a `given Pipeline[Wrapper]` we can call the `.andThen` method to represent this sequential piping.
    */
  type Context[+_]
  given sequentialContext: Sequential[Context] = scala.compiletime.deferred

  def interp(expr: Expr, env: Env): Context[Value]

}

trait Arithmetic extends Base with Expressions {

  def interpArithmetic(expr: ArithmeticExpr, env: Env): Context[Value | Num] = {

    def binOp(lhs: Expr, rhs: Expr, combine: (Int, Int) => Int): Context[Value | Num] =
      interp(lhs, env).andThen: lv =>
          interp(rhs, env).andThen: rv =>
              (lv, rv) match {
                case (Num(m), Num(n)) => Sequential.ret(Num(combine(m, n)))
                case _                => sys.error(s"Can do arithmetic with numbers, but got $lhs and $rhs")
              }

    expr match {
      case Num(n) => Sequential.ret(Num(n))

      case Add(lhs, rhs)  => binOp(lhs, rhs, _ + _)
      case Mult(lhs, rhs) => binOp(lhs, rhs, _ * _)
      case Sub(lhs, rhs)  => binOp(lhs, rhs, _ - _)

      case If0(testExpr, thenExpr, elseExpr) =>
        interp(testExpr, env).andThen:
            case Num(n) =>
              if n == 0
              then interp(thenExpr, env)
              else interp(elseExpr, env)
            case other => sys.error(s"can only test numbers, but got: $other")

    }
  }
}

trait Functions extends Base with Expressions {

  given storeEnv: EnvironmentStoreApi[Value, Env, Context] = scala.compiletime.deferred

  case class Closure(param: String, body: Expr, env: Env)

  def interpFunctions(expr: FunExpr | Let, env: Env): Context[Value | Closure] = expr match
      case Id(name) => env.lookup(name)

      case Let(boundId, namedExpr, boundBody) =>
        interp(namedExpr, env).andThen: namedValue =>
            env.bind(boundId, namedValue).andThen: newEnv =>
                interp(boundBody, newEnv)

      case Fun(param, body) => Sequential.ret(Closure(param, body, env))

      case App(funExpr, argExpr) =>
        interp(funExpr, env).andThen: funV =>
            val env2 = env.reachable(funV)
            interp(argExpr, env2).andThen: argV =>
                funV match {
                  case Closure(fParam, fBody, fEnv) =>
                    fEnv.stack(env2).bind(fParam, argV).andThen: funEnv =>
                        interp(fBody, funEnv)
                  case _ => sys.error(s"can only apply functions, but got: $funV")
                }

}

trait Misc extends Base with Expressions {

  given storeEnv: EnvironmentStoreApi[Value | Dummy, Env, Context] = scala.compiletime.deferred

  type MiscExpr = Seqn | LetRec | SetId

  case class Dummy()

  def interpMisc(expr: MiscExpr, env: Env): Context[Value] = expr match

      case Seqn(args*) =>
        args match
            // args is a `Seq`
            case Seq(first, others*) =>
              // the first element is special, because there is nothing else to do before, while the rest requires andThen
              others.foldLeft(interp(first, env)): (acc, expr) =>
                  acc.andThen: _ =>
                      interp(expr, env)

      case SetId(id, valExpr) =>
        interp(valExpr, env).andThen: value =>
            env.update(id, value).andThen: _ =>
                Sequential.ret(value)

      /** In our stateful language, we do not require mutation from the
        * host language to implement cyclic environments.
        */
      case LetRec(boundId, namedExpr, boundBody) =>
        env.bind(boundId, Dummy()).andThen: newEnv =>
            interp(namedExpr, newEnv).andThen: namedVal =>
                newEnv.update(boundId, namedVal).andThen: updatedEnv =>
                    interp(boundBody, updatedEnv)
}

trait Boxes extends Base with Expressions {

  def store: StoreApi[Value | Box, Context]
  given rootedEnvironment: HasRoots[Env] = scala.compiletime.deferred

  type Location = Int

  case class Box(location: Location)

  def interpBoxes(expr: BoxExpr, env: Env): Context[Value | Box] =
    expr match {

      case NewBox(boxExpr) =>
        interp(boxExpr, env).andThen: boxV =>
            store.malloc(boxV, env.roots).andThen: location =>
                Sequential.ret(Box(location))

      case SetBox(boxExpr, valueExpr) =>
        interp(boxExpr, env).andThen: boxV =>
            interp(valueExpr, env).andThen: value =>
                boxV match {
                  case Box(loc) => store.update(loc, value).andThen(_ => Sequential.ret(value))
                  case _        => sys.error(s"can only set to boxes, but got: $boxV")
                }

      case OpenBox(boxExpr) =>
        interp(boxExpr, env).andThen:
            case Box(loc) => store.lookup(loc)
            case other    => sys.error(s"can only open boxes, but got: $other")

    }

}

trait Booleans extends Base with Expressions {

  def interpBooleans(expr: BooleanExpr, env: Env): Context[Value | True.type | False.type] = expr match {
    case True          => Sequential.ret(True)
    case False         => Sequential.ret(False)
    case And(lhs, rhs) => interp(lhs, env).andThen:
          case False => Sequential.ret(False)
          case True  => interp(rhs, env)
    case Or(lhs, rhs) => interp(lhs, env).andThen:
          case True  => Sequential.ret(True)
          case False => interp(rhs, env)
    case Not(e) => interp(e, env).andThen:
          case True  => Sequential.ret(False)
          case False => Sequential.ret(True)

    // Note: this uses Scala’s built in equality on case classes (a form of meta interpretation).
    case Eq(lhs, rhs) =>
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      if lv == rv then Sequential.ret(True) else Sequential.ret(False)

    case If(test, thenBody, elseBody) =>
      interp(test, env).andThen {
        case True  => interp(thenBody, env)
        case False => interp(elseBody, env)
        case other => sys.error(s"if requires a boolean value, but got $other")
      }
  }
}

trait Records extends Base with Expressions {

  // TODO: these are immutable records, may consider mutable ones instead?
  case class RecordValue(fields: Map[String, Value])

  def interpRecords(expr: RecordExpr, env: Env): Context[Value | RecordValue] = expr match
      case Record(fields) =>
        val (ids, values)                       = fields.toList.unzip
        val wrappedFields: List[Context[Value]] = values.map(expr => interp(expr, env))
        Sequential.sequence(wrappedFields).andThen: mappedValues =>
            Sequential.ret(RecordValue(ids.zip(mappedValues).toMap))
      case RecordProjection(recordExpr, label) =>
        interp(recordExpr, env).andThen:
            case RecordValue(fields) => Sequential.ret(fields(label))
            case other               => sys.error(s"cannot lookup $label in $other")

}

trait AlgebraicData extends Base, Expressions {

  given storeEnv: EnvironmentStoreApi[Value | Constructor, Env, Context] = scala.compiletime.deferred

  case class Constructor(name: String, args: List[Value])

  def interpAlgebraicData(expr: AlgebraicDataExpr, env: Env): Context[Value | Constructor] = expr match
      case Data(name, args) =>
        Sequential.sequence(args.map(arg => interp(arg, env))).andThen: mappedArgs =>
            Sequential.ret(Constructor(name, mappedArgs))

      case Match(expr, cases) => interp(expr, env).andThen {
          case value @ Constructor(name, argValues) =>
            val interpretedValue: Option[Context[Value]] = cases.collectFirst {
              case (caseName, caseStrings, caseExpr) if caseName == name && argValues.length == caseStrings.length =>
                val newEnv = (caseStrings zip argValues).foldLeft(Sequential.ret(env)): (env, tuple) =>
                    env.andThen: env =>
                        env.bind.tupled(tuple)
                newEnv.andThen: newEnv =>
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
