package homework.assignment05continuations

import homework.assignment05continuations.Task3.*

import scala.language.implicitConversions
import scala.util.boundary

class Task3Test extends munit.FunSuite {
  implicit def symbolToFLBE(symbol: String): Id = Id(symbol)
  implicit def boolToFLBE(n: Boolean): Bool     = Bool(n)

  def runInterp(expr: FLBE): Value = {
    boundary {
      interp(expr, Map.empty, boundary.break(_))
    }
  }

  test("1") { assertEquals(runInterp(And(true, true)), Bool(true)) }

  test("2") { assertEquals(runInterp(Or(false, false)), Bool(false)) }

  test("3") { assertEquals(runInterp(Let("x", true, Let("y", false, Or("y", "x")))), Bool(true)) }

  test("4") { assertEquals(runInterp(Let("y", Fun("x", Not("x")), App("y", true))), Bool(false)) }

  test("5") { assertEquals(runInterp(Let("x", false, If(Not("x"), "x", true))), Bool(false)) }

  test("6") {
    assertEquals(
      runInterp(Let("a", true, Let("b", false, Let("f", Fun("x", And(And("x", "a"), "b")), "f")))),
      Closure("x", And(And("x", "a"), "b"), Map("a" -> Bool(true), "b" -> Bool(false)))
    )
  }

  test("7") { intercept[RuntimeException] { runInterp(Let("b", true, "a")) } }

  test("8") { intercept[RuntimeException] { runInterp(Let("f", Fun("y", true), And("f", true))) } }

  test("9") { intercept[RuntimeException] { runInterp(Not("x")) } }

  test("10") { assertEquals(runInterp(And(true, BindCC("k", Or(true, App("k", false))))), Bool(false)) }

  test("11") { assertEquals(runInterp(BindCC("k", Not(false))), Bool(true)) }

  test("12") { assertEquals(runInterp(BindCC("k", Let("y", Fun("x", Not("x")), App("y", true)))), Bool(false)) }

  test("13") { assertEquals(runInterp(Let("x", true, Not(BindCC("c", Let("x", false, App("c", "x")))))), Bool(true)) }

  test("14") {
    assertEquals(
      runInterp(Let("x", true, Let("y", true, And("x", BindCC("k", Let("y", false, App("k", "y"))))))),
      Bool(false)
    )
  }

  test("15") {
    assertEquals(runInterp(And(true, BindCC("a", And(true, BindCC("b", App("a", App("b", false))))))), Bool(false))
  }

  test("16") {
    assertEquals(
      runInterp(And(true, BindCC("k", Let("f", Fun("v", And("v", App("k", true))), App("f", false))))),
      Bool(true)
    )
  }

  test("boundary") {
    assertEquals(
      runInterp(Boundary(
        Let("any", Break(Bool(true)), Bool(false))
      )),
      Bool(true)
    )
  }

  test("boundary 2") {
    assertEquals(
      runInterp(Boundary(
        Let(
          "_",
          Boundary(
            Let("any", Break(Bool(true)), Bool(false))
          ),
          Break(false)
        )
      )),
      Bool(false)
    )
  }

  test("boundary 3") {
    assertEquals(
      runInterp(Boundary(
        Let(
          "_",
          Boundary(
            Let("any", Break(Bool(true)), Bool(false))
          ),
          Break(Break(false))
        )
      )),
      Bool(false)
    )
  }
}
