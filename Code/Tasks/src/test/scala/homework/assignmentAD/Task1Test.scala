package homework.assignmentAD

import homework.assignmentAD.Task1.*
import munit.FunSuite

class Task1Test extends FunSuite {

  private val N   = summon[Number[Dual]]
  private val eps = 1e-10

  private def assertDualApprox(obt: Dual, exp: Dual, delta: Double = eps): Unit = {
    assertEqualsDouble(obt.x, exp.x, delta)
    assertEqualsDouble(obt.dx, exp.dx, delta)
  }

  test("neg") {
    val a   = Dual(3.0, -4.5)
    val got = N.neg(a)
    assertDualApprox(got, Dual(-3.0, 4.5))
  }

  test("sub") {
    val a   = Dual(3.0, 1.0)
    val b   = Dual(2.0, 0.25)
    val got = N.sub(a, b)
    assertDualApprox(got, Dual(1.0, 0.75))
  }
  test("exp") {
    val x   = 0.3
    val a   = Dual(x, 2.0)
    val got = N.exp(a)
    assertDualApprox(got, Dual(1.3498588075760032, 2.6997176151520064))
  }

  test("log") {
    val x   = 2.5
    val a   = Dual(x, -1.2)
    val got = N.log(a)
    assertDualApprox(got, Dual(0.9162907318741551, -0.48))
  }

  test("sqrt") {
    val x   = 4.0
    val a   = Dual(x, 3.0)
    val got = N.sqrt(a)
    assertDualApprox(got, Dual(2.0, 0.75))
  }

  test("tan") {
    val x   = 0.2 // avoid points where cos(x) ~ 0
    val a   = Dual(x, 0.7)
    val got = N.tan(a)
    assertDualApprox(got, Dual(0.2027100355086725, 0.7287639509471491))
  }

  test("log") {
    val a   = Dual(0.1, -0.4)
    val got = N.log(N.exp(a))
    assertDualApprox(got, a, delta = 1e-9)
  }

  test("sqrt") {
    val a   = Dual(9.0, 0.6)
    val s   = N.sqrt(a)
    val got = N.mul(s, s)
    assertDualApprox(got, a, delta = 1e-9)
  }
}
