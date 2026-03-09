error id: 4EB63853D299C344DA18CA84725E74DA
file://<HOME>/uni/copl/Tasks/Exercise%20Bundle%2003/Code/Tasks/src/main/scala/homework/assignment03functions/part1/Fibonacci.scala
### java.lang.AssertionError: assertion failed: file://<HOME>/uni/copl/Tasks/Exercise%20Bundle%2003/Code/Tasks/src/main/scala/homework/assignment03functions/part1/Fibonacci.scala: 386 >= 386

occurred in the presentation compiler.



action parameters:
offset: 386
uri: file://<HOME>/uni/copl/Tasks/Exercise%20Bundle%2003/Code/Tasks/src/main/scala/homework/assignment03functions/part1/Fibonacci.scala
text:
```scala
package homework.assignment03functions.part1

import interpreters.conditionals.ConditionalsSubstitution.*

object Fibonacci {

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  // Define your function here
  val fibDefs: Map[String, FunDef] = 
    Map("fib" -> FunDef("n", "n")) 

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

java.lang.AssertionError: assertion failed: file://<HOME>/uni/copl/Tasks/Exercise%20Bundle%2003/Code/Tasks/src/main/scala/homework/assignment03functions/part1/Fibonacci.scala: 386 >= 386