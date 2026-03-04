package modularized

import interpreters.conditionals.ConditionalsSubstitution.*

class ConditionalsTest extends munit.FunSuite {
  val funDefs: Map[String, FunDef] = Map(
    "f" -> FunDef("n", App("g", Add("n", 5))),
    "g" -> FunDef("n", Sub("n", 1))
  )

  test("subst If0") {
    assertEquals(subst(If0("x", "x", "x"), "x", 3), If0(3, 3, 3))
  }

  val testcases: Seq[(String, Int, Expr, Map[String, FunDef])] = List(
    ("App", 9, App("f", 5), funDefs),
    ("If0 true", 1, If0(0, 1, 2), Map.empty),
    ("If0 false", 2, If0(1, 1, 2), Map.empty),
    ("If0, Let", 3, If0(0, Let("x", 3, "x"), Let("x", 4, "x")), Map.empty),
    ("If0, App", 2, If0(App("f", 0), 1, 2), funDefs),
    ("If0, Let, Sub", 2, If0(Let("x", 3, Sub("x", 3)), Let("x", 2, "x"), Let("x", 5, "x")), Map.empty),
    ("App, Add", 7, App("f", Add(1, 2)), funDefs),
    ("Let, Sub", 0, Let("y", 2, Sub("y", "y")), funDefs),
    ("binding in cond", 2, Let("x", 42, If0("x", 1, 2)), Map.empty),
    ("binding in then", 42, Let("x", 42, If0(0, "x", 10)), Map.empty),
    ("binding in else", 42, Let("x", 42, If0(1, 10, "x")), Map.empty),
    ("binding in all", 3, Let("x", 3, If0("x", "x", "x")), Map.empty),
    ("nested If0", 5, If0(If0(0, 1, 2), If0(0, 3, 4), If0(0, 5, 6)), Map.empty),
    ("nested If0, App", 2, If0(App("f", If0(App("f", 0), 1, 2)), 1, 2), funDefs),
    ("nested If0, App, Let 1", 2, If0(App("f", If0(Let("x", 0, "x"), 1, 2)), 1, 2), funDefs),
    ("nested If0, App, Let 2", 42, If0(App("g", If0(Let("x", 0, "x"), 1, 2)), 42, 0), funDefs),
  )

  testcases.foreach { case (title, expectation, expr, context: Map[String, FunDef]) =>
    test(title) {
      assertEquals(interp(expr, context), expectation)
    }
  }
}
