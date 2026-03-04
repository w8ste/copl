package homework.assignment04recursionfix

import homework.assignment04recursionfix.ScalaRecursionWithFixpointCombinator.*
import munit.FunSuite

class ScalaRecursionWithFixpointCombinatorTest extends FunSuite {

  val lengths: List[(List[Int], Int)] =
    List((List(), 0), (List(1), 1), (List(5, 1), 2), (List(8, 3, 6), 3), (List(52), 1))

  lengths.foreach { case (n: List[Int], r: Int) =>
    test(s"length($n)") {
      assertEquals(lengthWithFix(n), r)
    }
  }

}
