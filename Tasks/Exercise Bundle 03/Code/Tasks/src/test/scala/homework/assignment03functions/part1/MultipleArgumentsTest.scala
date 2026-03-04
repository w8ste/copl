package homework.assignment03functions.part1

import homework.assignment03functions.part1.MultipleArguments.*

class MultipleArgumentsTest extends munit.FunSuite {

  // the usual syntactic convenience

  import scala.language.implicitConversions

  implicit def symbolToExpr(symbol: String): Id = Id(symbol)
  implicit def intToExpr(n: Int): Num           = Num(n)

  // --------------------------------------------------------
  // --- REMOVE THIS DEFINITION WHEN DOING THE ASSIGNMENT ---
  // --------------------------------------------------------
  // it currently exists to make the tests compile, even though the types don’t match up
  // if you remove it you will get compiler errors making the task easier
  implicit def hackToMakeTestsCompile[A](list: List[A]): A =
    sys.error("remove this method to get compiler errors while implementing the task")

  val funDefs: Map[String, FunDef] =
    Map("f" -> FunDef("f", List("n"), App("g", List(Add("n", 5)))), "g" -> FunDef("g", List("n"), Sub("n", 1)))

  test("interp app0") {
    assertEquals(interp(App("c", List()), Map("c" -> FunDef("c", List(), 2))), 2)
  }

  test("interp app1") {
    assertEquals(interp(App("f", List[Expr](5)), funDefs), 9)
  }

  test("interp app2") {
    assertEquals(interp(App("f", List[Expr](1, 2)), Map("f" -> FunDef("f", List("x", "y"), Add("x", "y")))), 3)
  }

  test("interp app3") {
    assertEquals(
      interp(App("g", List[Expr](1, 2, 3)), Map("g" -> FunDef("g", List("x", "y", "z"), Add("x", Sub("y", "z"))))),
      0
    )
  }

  test("interp let") {
    assertEquals(interp(Let("y", 2, "y"), Map.empty[String, FunDef]), 2)
  }

  test("interp let app2") {
    assertEquals(
      interp(Let("y", 2, App("f", List[Expr]("y", 1))), Map("f" -> FunDef("f", List("x", "y"), Sub("x", "y")))),
      1
    )
  }

  test("interp let let app2") {
    assertEquals(
      interp(
        Let("y", 2, Let("x", 1, App("f", List[Expr]("y", "x")))),
        Map("f" -> FunDef("f", List("x", "y"), Sub("x", "y")))
      ),
      1
    )
  }

  test("interp app, expr in arg") {
    assertEquals(
      interp(App("f", List[Expr](Let("x", 2, Sub("x", 1)), 2)), Map("f" -> FunDef("f", List("x", "y"), Add("x", "y")))),
      3
    )
  }

  test("interp app1, let in add") {
    assertEquals(interp(App("f", List[Expr](5)), Map("f" -> FunDef("f", List("x"), Add(Let("x", 2, "x"), "x")))), 7)
  }

  test("interp app2, let in add, 1") {
    assertEquals(
      interp(App("f", List[Expr](1, 2)), Map("f" -> FunDef("f", List("x", "y"), Add(Let("x", 2, "x"), "y")))),
      4
    )
  }

  test("interp app2, let in add, 2") {
    assertEquals(
      interp(App("f", List[Expr](5, 2)), Map("f" -> FunDef("f", List("x", "y"), Add(Let("x", 2, "x"), "y")))),
      4
    )
  }

  test("interp complex") {
    assertEquals(
      interp(
        Let(
          "y",
          App("A", List()),
          Let(
            "x",
            Sub("y", 25),
            App(
              "f",
              List[Expr](
                "x",
                Sub("y", App("f", List(Sub(App("f", List[Expr]("x", 1)), 1), App("f", List[Expr](20, 3)))))
              )
            )
          )
        ),
        Map(
          "f" -> FunDef("f", List("x", "y"), Add("x", "y")),
          "A" -> FunDef("A", List(), 65)
        )
      ),
      42
    )
  }

}
