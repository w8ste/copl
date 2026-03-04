error id: 4EB63853D299C344DA18CA84725E74DA
file://<HOME>/uni/copl/copl-2025-main/Tasks/Exercise%20Bundle%2002/Code/Tasks/src/main/scala/exercises/ex02substitution/TaskLetAEInterpreter.scala
### java.lang.AssertionError: assertion failed: file://<HOME>/uni/copl/copl-2025-main/Tasks/Exercise%20Bundle%2002/Code/Tasks/src/main/scala/exercises/ex02substitution/TaskLetAEInterpreter.scala: 3723 >= 3723

occurred in the presentation compiler.



action parameters:
offset: 3723
uri: file://<HOME>/uni/copl/copl-2025-main/Tasks/Exercise%20Bundle%2002/Code/Tasks/src/main/scala/exercises/ex02substitution/TaskLetAEInterpreter.scala
text:
```scala
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

  def progSize(p: LAE): Int =
    p match {
      case Num(n) => 1
      case Add(lhs, rhs) => {
        1 + progSize(lhs) + progSize(rhs)
      }
      case Let(n, ne, b) => {
        1 + progSize(ne) + progSize(b)
      }
      case Id(n) => 1
    }


  /*
   * 2) Write a function 'freevars' that collects the free (unbound) variables of a program.
   */

  def freeVars(p: LAE): Set[String] =
    p match {
      case Num(_) => Set()
      case Id(name) => Set(name)
      case Add(lhs, rhs) => freeVars(lhs) ++ freeVars(rhs)
      case Let(n, ne, b) => freeVars(ne) ++ (freeVars(b) - n)
    }

  

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
   * 0) Is this interpreter eager or lazy? lazy
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
   *    Alpha equivalence
   */
  val program1 =
    Let("x", Num(2), Let("y", Num(3), Add(Id("x"), Id("y"))))

  val program2 =
    Let("x", Num(2), Let("y2", Num(3), Add(Id("x"), Id("y2"))))

  val program3 = Num(5)

  

  /*
   * 2) Would you say that the programs program1b and program2b are equal?
   *    What is the problem? 
   */
  val program1b =
    Let("x", Id("y"), Let("y", Num(3), Add(Id("x"), Id("y"))))

  val program2b =
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
      if (boundId == substId)
        Let(boundId, substNamedExpr, boundExpr)
      else if(freeVars(boundExpr).contains(boundId))
        val fresh = deriveFreshName(boundId, freeVars(boundExpr) ++ freeVars(value) + substId)
        val renamed = subst(boundExpr, boundId, Id(fresh))
        Let(fresh, substNamedExpr, subst(renamed, substId, value))
      else
        Let(boundId, substNamedExpr, subst(boundExpr, substId, value))

    case Id(name) =>
      if substId == name
      then value
      else expr
  }

  

}
@@
```


presentation compiler configuration:
Scala version: 2.13.16
Classpath:
<HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.16/scala-library-2.13.16.jar [exists ]
Options:





#### Error stacktrace:

```
scala.reflect.internal.util.SourceFile.position(SourceFile.scala:34)
	scala.tools.nsc.CompilationUnits$CompilationUnit.position(CompilationUnits.scala:136)
	scala.meta.internal.pc.WithCompilationUnit.<init>(WithCompilationUnit.scala:20)
	scala.meta.internal.pc.WithSymbolSearchCollector.<init>(PcCollector.scala:352)
	scala.meta.internal.pc.PcDocumentHighlightProvider.<init>(PcDocumentHighlightProvider.scala:12)
	scala.meta.internal.pc.ScalaPresentationCompiler.$anonfun$documentHighlight$1(ScalaPresentationCompiler.scala:560)
```
#### Short summary:

java.lang.AssertionError: assertion failed: file://<HOME>/uni/copl/copl-2025-main/Tasks/Exercise%20Bundle%2002/Code/Tasks/src/main/scala/exercises/ex02substitution/TaskLetAEInterpreter.scala: 3723 >= 3723