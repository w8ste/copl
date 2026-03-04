package exercises.ex01basics

object Exercise01 {
  /* Main method */
  def main(args: Array[String]): Unit =
    println("Hello World!")
    // ...
}

/*
  Task 1: Factorial function in Scala

  Write one factorial function in each of the following styles.
	a) recursion, no mutation
	b) while loop with accumulator variable
	c) folding, no mutation
 */

object Factorial {

  def factorialA(n: Int): Int = ???

  def factorialB(n: Int): Int = ???

  def factorialC(n: Int): Int = ???

}

/*
 * Task 2: Higher-Order Functions
 *
 * a) Arithmetics
 */
object Arithmetics {

  /*
   * 1) Write a recursive function 'sum' that sums up the elements of a list of numbers.
   */
  def sum(l: List[Int]): Int = ???

  /*
   * 2) Write a recursive function 'prod' that builds the product of the elements in list of numbers.
   */
  def prod(l: List[Int]): Int = ???

  /*
   * 3) Can you recognize similarities between 'sum' and 'prod'? Write a higher-order function 'fold' that
   *    abstracts over these similarities. Reimplement 'sum' and 'prod' in terms of 'fold'.
   */

  // def fold(...): ... =
  //   ...

  def sumFold(l: List[Int]): Int = ???

  def prodFold(l: List[Int]): Int = ???

}

/*
 * b) Trees
 */
object Trees {

  trait Tree[A]
  case class Leaf[A](a: A)                   extends Tree[A]
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  /*
   * 1) Write a recursive function 'sum' that sums up the elements of a tree of numbers.
   */

  def sumTree(tree: Tree[Int]): Int = ???

  /*
   * 2) Write a recursive function 'collectTree' that collects all elements stored in the leaves of a tree.
   */

  def collectTree[A](tree: Tree[A]): Set[A] = ???

  /*
   * 3) Can you recognize similarities between 'sumTree' and 'collectTree'? Write a higher-order function 'foldTree' that
   *    abstracts over these similarities. Reimplement 'sumTree' and 'collectTree' in terms of 'foldTree'.
   */

  // def foldTree(...): ... =
  //   ...

  def sumTreeFold(tree: Tree[Int]): Int = ???

  def collectTreeFold[A](tree: Tree[A]): Set[A] = ???

}

/*
  Task 3: Scala code style

  Take a look at Scala coding styleguide:
  https://docs.scala-lang.org/style/

  Refactor your code above accordingly.

  Or live the simple life, your choice:
  https://scalameta.org/scalafmt/

 */
