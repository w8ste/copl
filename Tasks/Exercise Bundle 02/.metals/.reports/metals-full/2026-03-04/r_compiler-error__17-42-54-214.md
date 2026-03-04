error id: 67F4FFA2057A906C23B1F1FAA08C2947
file://<HOME>/uni/copl/copl-2025-main/Tasks/Exercise%20Bundle%2002/Code/Tasks/src/main/scala/homework/assignment02let/DeBruijn.scala
### java.lang.StringIndexOutOfBoundsException: Range [1688, 1688 + -6) out of bounds for length 1741

occurred in the presentation compiler.



action parameters:
offset: 1686
uri: file://<HOME>/uni/copl/copl-2025-main/Tasks/Exercise%20Bundle%2002/Code/Tasks/src/main/scala/homework/assignment02let/DeBruijn.scala
text:
```scala
package homework.assignment02let

object DeBruijn {
  sealed trait Expr
  case class NumDB(n: Int)                      extends Expr
  case class AddDB(lhs: Expr, rhs: Expr)        extends Expr
  case class SubDB(lhs: Expr, rhs: Expr)        extends Expr
  case class LetDB(namedExpr: Expr, body: Expr) extends Expr
  case class RefDB(n: Int)                      extends Expr

  sealed trait LAE
  case class Num(n: Int)                                     extends LAE
  case class Add(lhs: LAE, rhs: LAE)                         extends LAE
  case class Sub(lhs: LAE, rhs: LAE)                         extends LAE
  case class Let(boundId: String, namedExpr: LAE, body: LAE) extends LAE
  case class Id(name: String)                                extends LAE

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  def convert(expr: LAE, subs: List[String] = List()): Expr = expr match {
    case Num(n)        => NumDB(n)
    case Add(lhs, rhs) => AddDB(convert(lhs, subs), convert(rhs, subs))
    case Sub(lhs, rhs) => SubDB(convert(lhs, subs), convert(rhs, subs))
    
      case Let(boundId, namedExpr, body) => LetDB(convert(namedExpr, subs), convert(body, boundId :: subs))
      case Id(name) => RefDB(subs.indexOf(name))
  }

  def interp(expr: Expr, subs: List[Int] = List()): Int = expr match {
    case NumDB(n)        => n
    case AddDB(lhs, rhs) => interp(lhs, subs) + interp(rhs, subs)
    case SubDB(lhs, rhs) => interp(lhs, subs) - interp(rhs, subs)
    
    case LetDB(namedExpr, body) => {
      val value: Int = interp(namedExpr, subs)
      interp(body, value :: s@@)
    }
    case RefDB(n)               => ??? 

  }
}

```


presentation compiler configuration:
Scala version: 2.13.16
Classpath:
<HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.16/scala-library-2.13.16.jar [exists ]
Options:





#### Error stacktrace:

```
java.base/jdk.internal.util.Preconditions$1.apply(Preconditions.java:55)
	java.base/jdk.internal.util.Preconditions$1.apply(Preconditions.java:52)
	java.base/jdk.internal.util.Preconditions$4.apply(Preconditions.java:213)
	java.base/jdk.internal.util.Preconditions$4.apply(Preconditions.java:210)
	java.base/jdk.internal.util.Preconditions.outOfBounds(Preconditions.java:98)
	java.base/jdk.internal.util.Preconditions.outOfBoundsCheckFromIndexSize(Preconditions.java:118)
	java.base/jdk.internal.util.Preconditions.checkFromIndexSize(Preconditions.java:397)
	java.base/java.lang.String.checkBoundsOffCount(String.java:4843)
	java.base/java.lang.String.rangeCheck(String.java:307)
	java.base/java.lang.String.<init>(String.java:303)
	scala.tools.nsc.interactive.Global.typeCompletions$1(Global.scala:1245)
	scala.tools.nsc.interactive.Global.completionsAt(Global.scala:1283)
	scala.meta.internal.pc.SignatureHelpProvider.$anonfun$treeSymbol$1(SignatureHelpProvider.scala:401)
	scala.Option.map(Option.scala:242)
	scala.meta.internal.pc.SignatureHelpProvider.treeSymbol(SignatureHelpProvider.scala:399)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCall$.unapply(SignatureHelpProvider.scala:216)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.visit(SignatureHelpProvider.scala:327)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.traverse(SignatureHelpProvider.scala:321)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.$anonfun$visit$5(SignatureHelpProvider.scala:357)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.$anonfun$visit$5$adapted(SignatureHelpProvider.scala:334)
	scala.collection.IterableOnceOps.foreach(IterableOnce.scala:619)
	scala.collection.IterableOnceOps.foreach$(IterableOnce.scala:617)
	scala.collection.AbstractIterable.foreach(Iterable.scala:935)
	scala.collection.IterableOps$WithFilter.foreach(Iterable.scala:905)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.$anonfun$visit$3(SignatureHelpProvider.scala:334)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.$anonfun$visit$3$adapted(SignatureHelpProvider.scala:333)
	scala.collection.IterableOnceOps.foreach(IterableOnce.scala:619)
	scala.collection.IterableOnceOps.foreach$(IterableOnce.scala:617)
	scala.collection.AbstractIterable.foreach(Iterable.scala:935)
	scala.collection.IterableOps$WithFilter.foreach(Iterable.scala:905)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.visit(SignatureHelpProvider.scala:333)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.traverse(SignatureHelpProvider.scala:321)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.fromTree(SignatureHelpProvider.scala:290)
	scala.meta.internal.pc.SignatureHelpProvider.$anonfun$signatureHelp$3(SignatureHelpProvider.scala:31)
	scala.Option.flatMap(Option.scala:283)
	scala.meta.internal.pc.SignatureHelpProvider.$anonfun$signatureHelp$2(SignatureHelpProvider.scala:29)
	scala.Option.flatMap(Option.scala:283)
	scala.meta.internal.pc.SignatureHelpProvider.signatureHelp(SignatureHelpProvider.scala:27)
	scala.meta.internal.pc.ScalaPresentationCompiler.$anonfun$signatureHelp$1(ScalaPresentationCompiler.scala:467)
```
#### Short summary:

java.lang.StringIndexOutOfBoundsException: Range [1688, 1688 + -6) out of bounds for length 1741