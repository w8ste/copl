package homework.assignment04recursionfix

object ScalaRecursionWithFixpointCombinator {
  // For reference
  def length(l: List[Int]): Int =
    if l.isEmpty then 0 else 1 + length(l.tail)

  // Scala fixpoint combinator
  def fix[A, B](f: (A => B) => A => B): A => B =
    (x: A) => f(fix(f))(x)

  def lengthWithFix(l: List[Int]): Int = ???

}
