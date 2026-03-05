error id: 4EB63853D299C344DA18CA84725E74DA
file://<HOME>/uni/copl/Tasks/Exercise%20Bundle%2002/Code/Tasks/src/main/scala/homework/assignment02let/Zip.scala
### java.lang.AssertionError: assertion failed: file://<HOME>/uni/copl/Tasks/Exercise%20Bundle%2002/Code/Tasks/src/main/scala/homework/assignment02let/Zip.scala: 748 >= 748

occurred in the presentation compiler.



action parameters:
offset: 748
uri: file://<HOME>/uni/copl/Tasks/Exercise%20Bundle%2002/Code/Tasks/src/main/scala/homework/assignment02let/Zip.scala
text:
```scala
package homework.assignment02let

object Zip {
  def zipop[A, B, C](f: (A, B) => C): (List[A], List[B]) => List[C] = {
    (l1: List[A], l2: List[B]) =>
      if l1.isEmpty || l2.isEmpty then
        List.empty
      else
        f(l1.head, l2.head) :: zipop(f)(l1.tail, l2.tail)
  }

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  
  def zipadd(lhs: List[Int], rhs: List[Int]): List[Int] = (lhs zip rhs).map {case (x, y) => x + y}
  def zip[A, B](lhs: List[A], rhs: List[B]): List[(A, B)] = lhs zip rhs
  def addMatrix(lhs: List[List[Int]], rhs: List[List[Int]]): List[List[Int]] = {
    (lhs zip rhs).map {(l1, r1) => zipadd(l1, r1)}
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

java.lang.AssertionError: assertion failed: file://<HOME>/uni/copl/Tasks/Exercise%20Bundle%2002/Code/Tasks/src/main/scala/homework/assignment02let/Zip.scala: 748 >= 748