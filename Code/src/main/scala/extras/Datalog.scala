package extras

import scala.language.implicitConversions

/*
Scala implementation of a blog describing a naive implementation of datalog [1].

[1] https://dodisturb.me/posts/2018-12-25-The-Essence-of-Datalog.html
 */
object Datalog {
  case class Rule(head: Atom, body: List[Atom]) {
    override def toString: String = {
      val bodys = if body.nonEmpty then s" :- ${body.mkString(", ")}"
      else ""
      s"$head$bodys."
    }
  }
  case class Atom(name: String, terms: List[Term]) {
    def :-(body: Atom*): Rule     = Rule(this, body.toList)
    override def toString: String = s"$name(${terms.mkString(", ")})"
  }
  sealed trait Term
  case class Var(name: Int) extends Term {
    override def toString: String = name.toString
  }
  case class Sym(value: String) extends Term {
    override def toString: String = value
  }

  type Substitution  = Map[Term, Term]
  type KnowledgeBase = Set[Atom]
  type Program       = List[Rule]

  case class AtomBuilder(s: String) {
    def apply(terms: Term*): Atom = Atom(s, terms.toList)
  }
  implicit class StringAtomBuilder(s: String) {
    def atom: AtomBuilder = AtomBuilder(s)
  }
  implicit def varSymbol(symbol: Int): Var   = Var(symbol)
  implicit def stringSym(value: String): Sym = Sym(value)
  implicit def atomRule(atom: Atom): Rule    = atom.:-()

  val emptySubstitution: Substitution = Map()

  def substitute(atom: Atom, substitution: Substitution): Atom = {
    atom.copy(terms = atom.terms.map {
      case s: Sym        => s
      case v @ Var(name) => substitution.getOrElse(name, v)
    })
  }

  def unify(left: Atom, right: Atom): Option[Substitution] = {
    if left.name != right.name then None
    else {

      def rec(l: List[Term], r: List[Term]): Option[Substitution] = (l, r) match {
        case (Nil, _) | (_, Nil)                => Some(emptySubstitution)
        case ((sl: Sym) :: rl, (sr: Sym) :: rr) => if sl == sr then rec(rl, rr) else None
        case ((vl: Var) :: rl, (sr: Sym) :: rr) =>
          for {
            partialSubst: Substitution <- rec(rl, rr)
            res                        <- partialSubst.get(vl) match {
              case Some(v) => if v != sr then None else Some(partialSubst)
              case None    => Some(partialSubst.updated(vl, sr))
            }
          } yield res
        case (_, (vr: Var) :: _) => sys.error("Second term assumed to be ground.")
      }

      rec(left.terms, right.terms)
    }
  }

  def evalAtom(knowledgeBase: KnowledgeBase)(
      atom: Atom,
      substitutions: List[Substitution]
  ): List[Substitution] = {
    for {
      substitution <- substitutions
      downToEarthAtom = substitute(atom, substitution)
      extension <- knowledgeBase.flatMap(unify(downToEarthAtom, _))
    } yield substitution ++ extension
  }

  def walk(knowledgeBase: KnowledgeBase, atoms: List[Atom]): List[Substitution] =
    atoms.foldRight(List(emptySubstitution))(evalAtom(knowledgeBase))

  def evalRule(knowledgeBase: KnowledgeBase, rule: Rule): KnowledgeBase =
    walk(knowledgeBase, rule.body).map(substitute(rule.head, _)).toSet

  def immediateConsequence(program: Program, knowledgeBase: KnowledgeBase): KnowledgeBase =
    knowledgeBase ++ program.flatMap(evalRule(knowledgeBase, _))

  def vars(atom: Atom): Set[Var] = atom.terms.collect { case v: Var => v }.toSet

  def isRangeRestricted(rule: Rule): Boolean =
    vars(rule.head).subsetOf(rule.body.flatMap(vars).toSet)

  def solve(program: Program): KnowledgeBase = {
    def step(knowledgeBase: KnowledgeBase): KnowledgeBase = {
      val nextKb = immediateConsequence(program, knowledgeBase)
      if nextKb == knowledgeBase then knowledgeBase
      else step(nextKb)
    }
    if program.forall(isRangeRestricted) then step(Set())
    else sys.error("Input program must be range restricted")
  }

  val edge: AtomBuilder = "edge".atom

  def main(args: Array[String]): Unit = {
    println {
      solve(List[Rule](
        edge("a", "b"),
        edge("b", "c"),
        edge("c", "d"),
        edge(1, 3) :- (edge(1, 2), edge(2, 3)),
      )).toString
    }
  }
}
