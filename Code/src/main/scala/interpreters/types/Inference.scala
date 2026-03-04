package interpreters.types

// This file is adapted from
// https://ps-tuebingen-courses.github.io/pl1-lecture-notes/27-type-inference/type-inference.html

object Inference {

  sealed trait Type
  case class FunType(from: Type, to: Type) extends Type
  case object NumType                      extends Type
  case class TypeVar(x: String)            extends Type

  def freeTypeVars(t: Type): Set[String] = t match {
    case FunType(f, t) => freeTypeVars(f) ++ freeTypeVars(t)
    case NumType       => Set.empty
    case TypeVar(x)    => Set(x)
  }

  // this variant of substitution is defined slightly differently than what we are used to
  // we pass it a name `name` and a type `param` and it returns a function that replaces all occurences of `name` with `param`
  def substitution(name: String, param: Type): Type => Type =
      def rec(body: Type): Type = body match
          case FunType(from, to) => FunType(rec(from), rec(to))
          case NumType           => NumType
          case TypeVar(y)        => if name == y then param else TypeVar(y)
      rec

  // This is the "Robinson unification algorithm"
  // It generates a substitution (similar to the above) that assigns
  // all type variables from `eq` such that the left and right side of the pairs are equal
  def unify(eq: List[(Type, Type)]): Type => Type = eq match {
    case Nil                                        => identity
    case (NumType, NumType) :: rest                 => unify(rest)
    case (FunType(f1, t1), FunType(f2, t2)) :: rest => unify((f1, f2) :: (t1, t2) :: rest)
    case (TypeVar(x), TypeVar(y)) :: rest if x == y => unify(rest)
    case (TypeVar(x), t) :: rest                    =>
      if freeTypeVars(t)(x) then sys.error(s"Occurs check: $x occurs in $t")
      val s               = substitution(x, t)
      val substitutedRest = rest.map((l, r) => (s(l), s(r)))
      // andThen is just function composition with the first function (here s) applied first
      s.andThen(unify(substitutedRest))
    case (t, TypeVar(x)) :: rest => unify((TypeVar(x), t) :: rest)
    case (t1, t2) :: rest        => sys.error(s"Cannot unify $t1 and $t2")
  }

  sealed trait Expr
  case class Num(n: Int)                                    extends Expr
  case class Id(name: String)                               extends Expr
  case class Add(lhs: Expr, rhs: Expr)                      extends Expr
  case class Fun(param: String, body: Expr)                 extends Expr // No type annotation!
  case class App(funExpr: Expr, argExpr: Expr)              extends Expr
  case class Let(name: String, namedExpr: Expr, body: Expr) extends Expr

  var tyvCount: Int           = 0
  def freshTypeVar(): TypeVar = {
    tyvCount += 1
    TypeVar(s"X${tyvCount.toString}")
  }

  def typeCheck(expr: Expr, context: Map[String, Type]): (List[(Type, Type)], Type) = expr match {
    case Num(n) => (List.empty, NumType)
    case Id(x)  => context.get(x) match
          case Some(t) => (List.empty, t)
          case _       => sys.error(s"free variable: $x")
    case Add(l, r) => (typeCheck(l, context), typeCheck(r, context)) match
          case ((eq1, t1), (eq2, t2)) =>
            ((t1, NumType) :: (t2, NumType) :: (eq1 ++ eq2), NumType)
    case Fun(arg, body) =>
      val argtype = freshTypeVar()
      val resbody = typeCheck(body, context + (arg -> argtype))
      (resbody._1, FunType(argtype, resbody._2))
    case App(f, a) =>
      val toType = freshTypeVar()
      (typeCheck(f, context), typeCheck(a, context)) match
          case ((eqs1, ft), (eqs2, at)) =>
            ((ft, FunType(at, toType)) :: (eqs1 ++ eqs2), toType)
    case Let(name, namedExpr, body) =>
      val (constraints1, paramType) = typeCheck(namedExpr, context)
      val (constraints2, typ)       = typeCheck(body, context + (name -> paramType))
      (constraints1 ++ constraints2, typ)
  }

  def doTypeCheck(e: Expr, gamma: Map[String, Type]): Type = {
    val (constraints, resType) = typeCheck(e, gamma)
    unify(constraints)(resType)
  }

}
