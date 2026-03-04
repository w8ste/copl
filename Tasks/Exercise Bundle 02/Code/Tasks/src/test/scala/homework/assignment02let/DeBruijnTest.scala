package homework.assignment02let

import homework.assignment02let.DeBruijn.*

import scala.language.implicitConversions

class DeBruijnTest extends munit.FunSuite {
  implicit def symToId(sym: String): Id  = Id(sym)
  implicit def intToNum(n: Int): Num     = Num(n)
  implicit def intToNumDB(n: Int): NumDB = NumDB(n)

  test("conv num literal") { assertEquals(convert(3), NumDB(3)) }
  test("conv addition") { assertEquals(convert(Add(3, 3)), AddDB(3, 3)) }
  test("conv subtraction") { assertEquals(convert(Sub(6, 4)), SubDB(6, 4)) }
  test("conv let ref") { assertEquals(convert(Let("x", 5, "x")), LetDB(5, RefDB(0))) }

  test("conv let let ref") {
    assertEquals(convert(Let("x", 1, Let("y", 2, "x"))), LetDB(1, LetDB(2, RefDB(1))))
  }

  test("conv let let add ref") {
    assertEquals(convert(Let("x", 1, Let("y", 2, Add("y", "x")))), LetDB(1, LetDB(2, AddDB(RefDB(0), RefDB(1)))))
  }

  test("conv deep ref") {
    assertEquals(
      convert(Let("x", 1, Let("y", 2, Let("z", 42, Let("a", 3, Let("b", 4, "z")))))),
      LetDB(1, LetDB(2, LetDB(42, LetDB(3, LetDB(4, RefDB(2))))))
    )
  }

  test("conv add with ref in lhs") {
    assertEquals(convert(Let("x", 1, Add("x", 2))), LetDB(1, AddDB(RefDB(0), 2)))
  }

  test("conv add with ref in rhs") {
    assertEquals(convert(Let("x", 3, Add(2, "x"))), LetDB(3, AddDB(2, RefDB(0))))
  }

  test("conv sub with ref in lhs") {
    assertEquals(convert(Let("x", 7, Sub("x", 2))), LetDB(7, SubDB(RefDB(0), 2)))
  }

  test("conv sub with ref in rhs") {
    assertEquals(convert(Let("x", 3, Sub(2, "x"))), LetDB(3, SubDB(2, RefDB(0))))
  }

  test("conv ref in named expression") {
    assertEquals(convert(Let("x", 42, Let("y", "x", "y"))), LetDB(42, LetDB(RefDB(0), RefDB(0))))
  }

  test("conv nested ref in named expression") {
    assertEquals(convert(Let("x", 20, Let("y", Add(22, "x"), "y"))), LetDB(20, LetDB(AddDB(22, RefDB(0)), RefDB(0))))
  }

  test("conv same ref, different depths") {
    assertEquals(
      convert(Let("x", Add(5, 5), Let("y", Sub("x", 3), Add("y", "y")))),
      LetDB(AddDB(5, 5), LetDB(SubDB(RefDB(0), 3), AddDB(RefDB(0), RefDB(0))))
    )
  }

  test("conv same symbols") {
    assertEquals(convert(Let("x", 13, Let("x", 42, "x"))), LetDB(13, LetDB(42, RefDB(0))))
  }

  test("conv same name for named expression") {
    assertEquals(convert(Let("x", 1, Let("x", 2, Add("x", "x")))), LetDB(1, LetDB(2, AddDB(RefDB(0), RefDB(0)))))
  }

  test("conv Nesting") {
    assertEquals(
      convert(Let("x", Sub(7, 3), Let("y", Sub("x", "x"), Add("x", "y")))),
      LetDB(SubDB(7, 3), LetDB(SubDB(RefDB(0), RefDB(0)), AddDB(RefDB(1), RefDB(0))))
    )
  }

  test("conv -1 for not found") {
    assertEquals(convert(Add(Let("x", 3, "x"), "x")), AddDB(LetDB(3, RefDB(0)), RefDB(-1)))
  }

  test("conv -1 for not found 2") {
    assertEquals(convert(Let("x", Add("x", 1), 2)), LetDB(AddDB(RefDB(-1), 1), 2))
  }

  //////////////////////////////////////////////////////////////////////////////////////

  test("interp num literal") { assertEquals(interp(3), 3) }
  test("interp addition") { assertEquals(interp(AddDB(3, 3)), 6) }
  test("interp subtraction") { assertEquals(interp(SubDB(6, 4)), 2) }
  test("interp let ref") { assertEquals(interp(LetDB(5, RefDB(0))), 5) }
  test("interp let let ref") { assertEquals(interp(LetDB(1, LetDB(2, RefDB(1)))), 1) }

  test("interp deep ref") {
    assertEquals(interp(LetDB(1, LetDB(2, LetDB(42, LetDB(3, LetDB(4, RefDB(2))))))), 42)
  }

  test("interp add with ref in lhs") {
    assertEquals(interp(LetDB(1, AddDB(RefDB(0), 2))), 3)
  }

  test("interp add with ref in rhs") {
    assertEquals(interp(LetDB(3, AddDB(2, RefDB(0)))), 5)
  }

  test("interp sub with ref in lhs") {
    assertEquals(interp(LetDB(7, SubDB(RefDB(0), 2))), 5)
  }

  test("interp sub with ref in rhs") {
    assertEquals(interp(LetDB(3, SubDB(2, RefDB(0)))), -1)
  }

  test("interp ref in named expression") {
    assertEquals(interp(LetDB(42, LetDB(RefDB(0), RefDB(0)))), 42)
  }

  test("interp nested ref in named expression") {
    assertEquals(interp(LetDB(20, LetDB(AddDB(22, RefDB(0)), RefDB(0)))), 42)
  }

  test("interp same ref, different depths") {
    assertEquals(interp(LetDB(AddDB(5, 5), LetDB(SubDB(RefDB(0), 3), AddDB(RefDB(0), RefDB(0))))), 14)
  }

  test("interp static scoping") {
    intercept[Exception] {
      interp(AddDB(LetDB(42, 0), RefDB(0)))
    }
  }

  test("interp recursive definition not allowed") {
    intercept[Exception] {
      interp(LetDB(RefDB(0), 1))
    }
  }

  test("allows referring to definitions after exiting a scope of another definition") {
    assertEquals(
      interp(LetDB(10, AddDB(LetDB(100, RefDB(0)), RefDB(0)))),
      110
    )
  }
}
