package exercises.ex02substitution

sealed trait LetArithmeticExpressions
type LAE = LetArithmeticExpressions
case class Num(n: Int)                                  extends LAE
case class Add(lhs: LAE, rhs: LAE)                      extends LAE
case class Let(name: String, namedExpr: LAE, body: LAE) extends LAE
case class Id(name: String)                             extends LAE

/*
 * Task: Analyzing Programs
 */
object ProgramAnalysis {

  /*
   * 1) Write a function 'progSize' that counts the number of AST nodes in a program.
   */

  def progSize(p: LAE): Int = ???

  /*
   * 2) Write a function 'freevars' that collects the free (unbound) variables of a program.
   */

  def freeVars(p: LAE): Set[String] = ???

  /** Generates an id that is not contained in the `known` set of ids.
    * The generated name is similar to `id` for better readability
    */
  def deriveFreshName(id: String, known: Set[String]): String =
      def rec(count: Int): String =
          val newName = s"$id-$count"
          if known.contains(newName)
          then rec(count + 1)
          else newName
      if known.contains(id) then rec(0) else id

}

/*
 * Task: Equivalence of Programs
 */
object LetAEInterpreter {

  /*
   * 0) Is this interpreter eager or lazy?
   */
  def interp(expr: LAE): Int = expr match {
    case Num(n)                             => n
    case Add(lhs, rhs)                      => interp(lhs) + interp(rhs)
    case Let(boundId, namedExpr, boundExpr) =>

      interp(subst(boundExpr, boundId, namedExpr))

    case Id(name) => sys.error("found unbound id " + name)
  }

  /*
   * 1) Would you say that the programs program1 and program2 are equal?
   *    What type of equivalences can you think of?
   */
  val program1: Let =
    Let("x", Num(2), Let("y", Num(3), Add(Id("x"), Id("y"))))

  val program2: Let =
    Let("x", Num(2), Let("y2", Num(3), Add(Id("x"), Id("y2"))))

  val program3: Num = Num(5)

  /*
   * 2) Would you say that the programs program1b and program2b are equal?
   *    What is the problem?
   */
  val program1b: Let =
    Let("x", Id("y"), Let("y", Num(3), Add(Id("x"), Id("y"))))

  val program2b: Let =
    Let("x", Id("y"), Let("y2", Num(3), Add(Id("x"), Id("y2"))))

  /*
   * 3) Update subst below such that program1b and program2b behave the same.
   * Hint: You only need to change the Let case.
   * The `freeVars` and `deriveFreshName` functions from above are useful to systematically fix the issue.
   */

  def subst(expr: LAE, substId: String, value: LAE): LAE = expr match {
    case Num(_) => expr

    case Add(lhs, rhs) =>
      Add(subst(lhs, substId, value), subst(rhs, substId, value))

    case Let(boundId, namedExpr, boundExpr) =>

      val substNamedExpr = subst(namedExpr, substId, value)
      if boundId == substId then
          Let(boundId, substNamedExpr, boundExpr)
      else
          Let(boundId, substNamedExpr, subst(boundExpr, substId, value))

    case Id(name) =>
      if substId == name
      then value
      else expr
  }

}
