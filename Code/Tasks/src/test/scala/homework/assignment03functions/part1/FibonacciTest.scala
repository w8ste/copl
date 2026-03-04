package homework.assignment03functions.part1

import homework.assignment03functions.part1.Fibonacci.*
import interpreters.conditionals.ConditionalsSubstitution.*

class FibonacciTest extends munit.FunSuite {

  val testcases: List[(Int, Num | Let | Add)] = List(
    (0, Num(0)),
    (1, Num(1)),
    (1, Num(2)),
    (3, Num(4)),
    (5, Num(5)),
    (8, Num(6)),
    (377, Num(14)),
    (13, Let("x", 6, Add("x", 1))),
    (21, Let("x", 6, Add("x", 2))),
    (34, Let("x", 7, Add(2, "x"))),
    (55, Add(4, 6)),
  )

  testcases.foreach { case (expectation, expr: Expr) =>
    test("test " + expr) {
      assertEquals(expectation, interp(App("fib", expr), fibDefs))
    }
  }

  test("sum of fibs") {
    assertEquals(89, interp(Add(App("fib", 9), App("fib", 10)), fibDefs))
  }
}
