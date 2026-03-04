package homework.assignmentAD

import homework.assignmentAD.Task2.*
import munit.FunSuite

class Task2Test extends FunSuite {

  private val eps = 1e-10

  private def assertDualApprox(obt: Value, exp: Dual, delta: Double = eps): Unit = {
    obt match {
      case d: Dual =>
        assertEqualsDouble(d.x, exp.x, delta)
        assertEqualsDouble(d.d, exp.d, delta)
      case v => fail(s"Expected Dual but got $v")
    }
  }

  test("sub") {
    val a    = Dual(3.0, 1.0)
    val b    = Dual(2.0, 0.25)
    val expr = Sub(Id("a"), Id("b"))
    val got  = interp(expr, Map("a" -> a, "b" -> b))
    assertDualApprox(got, Dual(1.0, 0.75))
  }

  test("exp") {
    val x    = 0.3
    val a    = Dual(x, 2.0)
    val expr = Exp(Id("x"))
    val got  = interp(expr, Map("x" -> a))
    assertDualApprox(got, Dual(1.3498588075760032, 2.6997176151520064))
  }

  test("sin") {
    val x    = 0.5
    val a    = Dual(x, 2.0)
    val expr = Sin(Id("x"))
    val got  = interp(expr, Map("x" -> a))
    assertDualApprox(got, Dual(math.sin(x), 2.0 * math.cos(x)))
  }

  test("cos") {
    val x    = 0.3
    val a    = Dual(x, -1.2)
    val expr = Cos(Id("x"))
    val got  = interp(expr, Map("x" -> a))
    assertDualApprox(got, Dual(math.cos(x), 1.2 * math.sin(x)))
  }

  test("add") {
    val a    = Dual(3.0, 1.0)
    val b    = Dual(2.0, 0.25)
    val expr = Add(Id("a"), Id("b"))
    val got  = interp(expr, Map("a" -> a, "b" -> b))
    assertDualApprox(got, Dual(5.0, 1.25))
  }

  test("mul") {
    val a    = Dual(3.0, 1.0)
    val b    = Dual(2.0, 0.25)
    val expr = Mul(Id("a"), Id("b"))
    val got  = interp(expr, Map("a" -> a, "b" -> b))
    assertDualApprox(got, Dual(6.0, 3.0 * 0.25 + 2.0 * 1.0))
  }

  test("let - exp") {
    val x    = 0.3
    val expr = Let("x", Num(x), Exp(Id("x")))
    val got  = interp(expr, Map.empty)
    assertDualApprox(got, Dual(1.3498588075760032, 0.0))
  }

  test("let - sin") {
    val x    = 0.5
    val expr = Let("x", Num(x), Sin(Id("x")))
    val got  = interp(expr, Map.empty)
    assertDualApprox(got, Dual(math.sin(x), 0.0))
  }

  test("function - identity") {
    val expr = App(Fun("x", Id("x")), Num(5.0))
    val got  = interp(expr, Map.empty)
    assertDualApprox(got, Dual(5.0, 0.0))
  }

  test("function - exp") {
    val x    = 0.3
    val expr = App(Fun("x", Exp(Id("x"))), Num(x))
    val got  = interp(expr, Map.empty)
    assertDualApprox(got, Dual(1.3498588075760032, 0.0))
  }

  test("nested function") {
    val expr = Let(
      "f",
      Fun("x", Mul(Id("x"), Num(2.0))),
      App(Id("f"), Num(3.0))
    )
    val got = interp(expr, Map.empty)
    assertDualApprox(got, Dual(6.0, 0.0))
  }

  test("complex expression") {
    val x    = 2.0
    val expr = Mul(Exp(Num(x)), Num(x))
    val got  = interp(expr, Map.empty)
    assertDualApprox(got, Dual(math.exp(x) * x, 0.0))
  }
}
