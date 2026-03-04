package homework.assignment10bmodularization

import homework.assignment10bmodularization.ModularInterpreter.*
import munit.FunSuite

class ModularInterpreterTest extends FunSuite {

  test("interp with boolean expressions") {
    assertEquals(interp(True), True)
    assertEquals(interp(False), False)

    val exprAnd = And(True, False)
    assertEquals(interp(exprAnd), False)

    val exprOr = Or(True, False)
    assertEquals(interp(exprOr), True)
  }

  test("interp with functions") {
    val func                      = Fun("x", Id("x"))
    val Closure(param, body, env) = interp(func): @unchecked

    assertEquals(param, "x")
    assertEquals(body, Id("x"))

    val app = App(func, True)
    assertEquals(interp(app), True)
  }

  test("interp with records") {
    val recordExpr  = Record(Map("a" -> True, "b" -> False))
    val recordValue = interp(recordExpr).asInstanceOf[RecordValue]

    assertEquals(recordValue.fields("a"), True)
    assertEquals(recordValue.fields("b"), False)

    val recordProjection = RecordProjection(recordExpr, "a")
    assertEquals(interp(recordProjection), True)
  }

  test("interp with conditionals") {
    val ifExpr   = If(True, Id("x"), Id("y"))
    val env: Env = Map("x" -> True, "y" -> False)

    assertEquals(interp(ifExpr, env), True)
  }
}
