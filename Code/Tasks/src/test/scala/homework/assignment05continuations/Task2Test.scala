package homework.assignment05continuations

import homework.assignment05continuations.Task2.*

import scala.language.implicitConversions

class Task2Test extends munit.FunSuite {
  implicit def symbolToFLBE(symbol: String): Id = Id(symbol)
  implicit def boolToFLBE(n: Boolean): Bool     = Bool(n)

  case class EndOfWorld(value: Value) extends Throwable

  def runInterp(expr: FLBE): Value = {
    try
      interp(expr, Map.empty, ret => throw EndOfWorld(ret))
    catch {
      case EndOfWorld(ret) => return ret
    }

    throw new NotImplementedError("we never want not reach this part of the code")
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

  test("7") {
    intercept[RuntimeException] { runInterp(Let("b", true, "a")) }
  }

  test("8") {
    intercept[RuntimeException] { runInterp(Let("f", Fun("y", true), And("f", true))) }
  }

  test("9") {
    intercept[RuntimeException] { runInterp(Not("x")) }
  }

  test("10") {
    intercept[RuntimeException] { runInterp(If(Fun("x", "x"), true, false)) }
  }

  test("11") {
    intercept[RuntimeException] { runInterp(Not(Fun("x", "x"))) }
  }

}
