package modularized.substitution

import modularized.Expressions

/** substitution implementation that already knows how to handle selected subsets of all [[KnownExpressions]] */
trait StandardSubstitution extends Substitution {

  type Expr >: Id <: KnownExpressions

  override def subst(expr: Expr, find: Expr, replace: Expr): Expr = substStandard(expr, find, replace)
  override def freeVariables(expr: Expr): Set[String]             = freeVariablesKnown(expr)

}

trait Substitution extends Expressions {

  /** Ids are special as it basically makes no sense to have substitution without Ids.
    * In this case, alpha renaming requires to substitute Ids, thus the lower bound here is necessary to say that
    * “at least we the Id case can be handled”
    */
  type Expr >: Id

  def freeVariables(expr: Expr): Set[String]

  def freeVariablesKnown(expr: KnownExpressions): Set[String] = expr match {
    case Id(x) => Set(x)

    // 0 args
    case Num(_) | True | False => Set.empty
    // 1 arg
    case Not(e)                 => freeVariables(e)
    case App(funName, e)        => freeVariables(e)
    case SetId(name, e)         => freeVariables(e)
    case NewBox(e)              => freeVariables(e)
    case OpenBox(e)             => freeVariables(e)
    case RecordProjection(e, n) => freeVariables(e)
    case AppFirstOrder(n, e)    => freeVariables(e)
    // 2 args
    case Add(lhs, rhs)    => freeVariables(lhs) ++ freeVariables(rhs)
    case Sub(lhs, rhs)    => freeVariables(lhs) ++ freeVariables(rhs)
    case Mult(lhs, rhs)   => freeVariables(lhs) ++ freeVariables(rhs)
    case And(lhs, rhs)    => freeVariables(lhs) ++ freeVariables(rhs)
    case Or(lhs, rhs)     => freeVariables(lhs) ++ freeVariables(rhs)
    case Eq(lhs, rhs)     => freeVariables(lhs) ++ freeVariables(rhs)
    case SetBox(lhs, rhs) => freeVariables(lhs) ++ freeVariables(rhs)
    // 3 args
    case If(e1, e2, e3)  => freeVariables(e1) ++ freeVariables(e2) ++ freeVariables(e3)
    case If0(e1, e2, e3) => freeVariables(e1) ++ freeVariables(e2) ++ freeVariables(e3)
    // varargs
    case Seqn(es*)      => es.iterator.flatMap(freeVariables).toSet
    case Data(n, args)  => args.iterator.flatMap(freeVariables).toSet
    case Record(fields) => fields.valuesIterator.flatMap(freeVariables).toSet

    // do bind names
    case Match(e, cases) =>
      freeVariables(e) ++
      cases.iterator.flatMap: (name, bindings, e2) =>
        freeVariables(e2) -- bindings
    case Fun(param, body)          => freeVariables(body) - param
    case TypedFun(param, _, body)  => freeVariables(body) - param
    case Let(name, named, body)    => freeVariables(named) ++ (freeVariables(body) - name)
    case LetRec(name, named, body) => (freeVariables(named) ++ freeVariables(body)) - name
  }

  /** Looks through an expression to find some subexpression and replace it */
  def subst(expr: Expr, find: Expr, replace: Expr): Expr

  // substitutes 2nd argument with 3rd argument in 1st argument. The resulting
  // expression contains no free instances of the 2nd argument.
  def substStandard(
      expr: StandardExpressions,
      find: StandardExpressions,
      replace: StandardExpressions
  ): StandardExpressions = {

    def freshName(n: String) = deriveFreshName(n, freeVariables(find) ++ freeVariables(replace))
    def substAlphaRenamed(expr: Expr, old: String, fresh: String) =
      val alphaRenamed = subst(expr, Id(old), Id(fresh))
      subst(alphaRenamed, find, replace)

    val res = expr match {
      case `find` => replace
      // 0 args
      case Num(_) | True | False | Id(_) => expr
      // 1 arg
      case Not(e)                 => Not(subst(e, find, replace))
      case SetId(name, e)         => SetId(name, subst(e, find, replace))
      case NewBox(e)              => NewBox(subst(e, find, replace))
      case OpenBox(e)             => OpenBox(subst(e, find, replace))
      case AppFirstOrder(n, e)    => AppFirstOrder(n, subst(e, find, replace))
      case RecordProjection(e, n) => RecordProjection(subst(e, find, replace), n)
      // 2 args
      case Add(lhs, rhs)    => Add(subst(lhs, find, replace), subst(rhs, find, replace))
      case Sub(lhs, rhs)    => Sub(subst(lhs, find, replace), subst(rhs, find, replace))
      case Mult(lhs, rhs)   => Mult(subst(lhs, find, replace), subst(rhs, find, replace))
      case And(lhs, rhs)    => And(subst(lhs, find, replace), subst(rhs, find, replace))
      case Or(lhs, rhs)     => Or(subst(lhs, find, replace), subst(rhs, find, replace))
      case Eq(lhs, rhs)     => Eq(subst(lhs, find, replace), subst(rhs, find, replace))
      case SetBox(lhs, rhs) => SetBox(subst(lhs, find, replace), subst(rhs, find, replace))
      case App(lhs, rhs)    => App(subst(lhs, find, replace), subst(rhs, find, replace))
      // 3 args
      case If0(test, thenBody, elseBody) =>
        If0(subst(test, find, replace), subst(thenBody, find, replace), subst(elseBody, find, replace))
      case If(test, thenBody, elseBody) =>
        If(subst(test, find, replace), subst(thenBody, find, replace), subst(elseBody, find, replace))
      // varargs
      case Seqn(es*)      => Seqn(es.map(e => subst(e, find, replace))*)
      case Data(n, es)    => Data(n, es.map(e => subst(e, find, replace)))
      case Record(fields) => Record(fields.transform((_, e) => subst(e, find, replace)))

      // do bind names
      case Match(e, cases) =>
        Match(
          subst(e, find, replace),
          cases.map: (name, ids, e) =>
            val (freshNames, alphaRenamed) = ids.foldLeft((List.empty[String], e)):
              case ((freshs, acc), id) =>
                val fresh = deriveFreshName(id, freeVariables(find) ++ freeVariables(replace) ++ (ids.toSet - id))
                ((fresh :: freshs), subst(acc, Id(id), Id(fresh)))
            (name, freshNames, subst(alphaRenamed, find, replace))
        )

      case Fun(param, body) =>
        val fresh = freshName(param)
        Fun(fresh, substAlphaRenamed(body, param, fresh))
      case TypedFun(param, t, body) =>
        val fresh = freshName(param)
        TypedFun(fresh, t, substAlphaRenamed(body, param, fresh))
      case Let(boundId, namedExpr, boundExpr) =>
        val fresh = freshName(boundId)
        Let(fresh, subst(namedExpr, find, replace), substAlphaRenamed(boundExpr, boundId, fresh))
      case LetRec(boundId, namedExpr, boundExpr) =>
        val fresh = freshName(boundId)
        LetRec(fresh, substAlphaRenamed(namedExpr, boundId, fresh), substAlphaRenamed(boundExpr, boundId, fresh))

    }
    // this cast is a bit unfortunate. Essentially, for any given `expr: Ex` the result type has to be `Ex`,
    // but the way types interact with pattern matching looses that information,
    // and the above just ends up being of type `AllExpressions` because some branches produce such values,
    // even if those branches become unreachable for certain input types
    res.asInstanceOf[StandardExpressions]
  }

  def deriveFreshName(id: String, known: Set[String]): String = {
    def rec(count: Int): String =
      val newName = s"$id-$count"
      if known.contains(newName)
      then rec(count + 1)
      else newName
    if known.contains(id) then rec(0) else id
  }
}
