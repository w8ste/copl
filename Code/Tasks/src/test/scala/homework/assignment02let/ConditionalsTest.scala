package homework.assignment02let

import homework.assignment02let.Conditionals.*

class ConditionalsTest extends munit.FunSuite {

  test("subst If0") {
    assertEquals(subst(If0("x", "x", "x"), "x", 3), If0(3, 3, 3))
  }

  val testcases: Seq[(String, Int, Expr)] = List(
    ("If0 true", 1, If0(0, 1, 2)),
    ("If0 false", 2, If0(1, 1, 2)),
    ("If0, Let", 3, If0(0, Let("x", 3, "x"), Let("x", 4, "x"))),
    ("If0, Let, Sub", 2, If0(Let("x", 3, Sub("x", 3)), Let("x", 2, "x"), Let("x", 5, "x"))),
    ("Let, Sub", 0, Let("y", 2, Sub("y", "y"))),
    ("binding in cond", 2, Let("x", 42, If0("x", 1, 2))),
    ("binding in then", 42, Let("x", 42, If0(0, "x", 10))),
    ("binding in else", 42, Let("x", 42, If0(1, 10, "x"))),
    ("binding in all", 3, Let("x", 3, If0("x", "x", "x"))),
    ("nested If0", 5, If0(If0(0, 1, 2), If0(0, 3, 4), If0(0, 5, 6))),
  )

  testcases.foreach { case (title, expectation, expr) =>
    test(title) {
      assertEquals(interp(expr), expectation)
    }
  }
}
