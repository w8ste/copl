package homework.assignmentAD

import homework.assignmentAD.Task3.*
import munit.FunSuite

class Task3Test extends FunSuite {

  private val eps = 1e-10

  private def assertDualApprox(obt: Value, expX: Double, delta: Double = eps): Unit = {
    obt match {
      case d: Dual => assertEqualsDouble(d.x, expX, delta)
      case v       => fail(s"Expected Dual but got $v")
    }
  }

  test("sub") {
    val expr     = Sub(Id("a"), Id("b"))
    val env: Env = Map("a" -> Dual(3.0, Tape.Const(0)), "b" -> Dual(2.0, Tape.Const(0)))
    val got      = interp(expr, env)
    assertDualApprox(got, 1.0)
  }

  test("exp") {
    val x        = 0.3
    val expr     = Exp(Id("x"))
    val env: Env = Map("x" -> Dual(x, Tape.Const(0)))
    val got      = interp(expr, env)
    assertDualApprox(got, 1.3498588075760032)
  }

  test("sin") {
    val x        = 0.5
    val expr     = Sin(Id("x"))
    val env: Env = Map("x" -> Dual(x, Tape.Const(0)))
    val got      = interp(expr, env)
    assertDualApprox(got, math.sin(x))
  }

  test("cos") {
    val x        = 0.3
    val expr     = Cos(Id("x"))
    val env: Env = Map("x" -> Dual(x, Tape.Const(0)))
    val got      = interp(expr, env)
    assertDualApprox(got, math.cos(x))
  }

  test("add") {
    val expr     = Add(Id("a"), Id("b"))
    val env: Env = Map("a" -> Dual(3.0, Tape.Const(0)), "b" -> Dual(2.0, Tape.Const(0)))
    val got      = interp(expr, env)
    assertDualApprox(got, 5.0)
  }

  test("mul") {
    val expr     = Mul(Id("a"), Id("b"))
    val env: Env = Map("a" -> Dual(3.0, Tape.Const(0)), "b" -> Dual(2.0, Tape.Const(0)))
    val got      = interp(expr, env)
    assertDualApprox(got, 6.0)
  }

  test("let - exp") {
    val x    = 0.3
    val expr = Let("x", Num(x), Exp(Id("x")))
    val got  = interp(expr, Map.empty)
    assertDualApprox(got, 1.3498588075760032)
  }

  test("let - sin") {
    val x    = 0.5
    val expr = Let("x", Num(x), Sin(Id("x")))
    val got  = interp(expr, Map.empty)
    assertDualApprox(got, math.sin(x))
  }

  test("function - identity") {
    val expr = App(Fun("x", Id("x")), Num(5.0))
    val got  = interp(expr, Map.empty)
    assertDualApprox(got, 5.0)
  }

  test("function - exp") {
    val x    = 0.3
    val expr = App(Fun("x", Exp(Id("x"))), Num(x))
    val got  = interp(expr, Map.empty)
    assertDualApprox(got, 1.3498588075760032)
  }

  test("nested function") {
    val expr = Let(
      "f",
      Fun("x", Mul(Id("x"), Num(2.0))),
      App(Id("f"), Num(3.0))
    )
    val got = interp(expr, Map.empty)
    assertDualApprox(got, 6.0)
  }

  test("complex expression") {
    val x    = 2.0
    val expr = Mul(Exp(Num(x)), Num(x))
    val got  = interp(expr, Map.empty)
    assertDualApprox(got, math.exp(x) * x)
  }

  // Derivative tests using derivative

  test("exp - derivative") {
    val (value, deriv) = derivative(Exp(Id("x")), 0.3)
    assertEqualsDouble(value, 1.3498588075760032, eps)
    assertEqualsDouble(deriv, 1.3498588075760032, eps)
  }

  test("sin - derivative") {
    val (value, deriv) = derivative(Sin(Id("x")), 0.5)
    assertEqualsDouble(value, math.sin(0.5), eps)
    assertEqualsDouble(deriv, 0.8775825618903728, eps)
  }

  test("cos - derivative") {
    val (value, deriv) = derivative(Cos(Id("x")), 0.3)
    assertEqualsDouble(value, math.cos(0.3), eps)
    assertEqualsDouble(deriv, -0.29552020666133955, eps)
  }

  test("x^2 - derivative") {
    val (value, deriv) = derivative(Mul(Id("x"), Id("x")), 3.0)
    assertEqualsDouble(value, 9.0, eps)
    assertEqualsDouble(deriv, 6.0, eps)
  }

  test("x^3 - derivative") {
    val (value, deriv) = derivative(Mul(Id("x"), Mul(Id("x"), Id("x"))), 3.0)
    assertEqualsDouble(value, 27.0, eps)
    assertEqualsDouble(deriv, 27.0, eps)
  }

  test("complex expression - derivative") {
    val (value, deriv) = derivative(Mul(Exp(Id("x")), Id("x")), 2.0)
    assertEqualsDouble(value, math.exp(2.0) * 2.0, eps)
    assertEqualsDouble(deriv, 22.16716829679195, eps)
  }
}
