package interpreters.continuations.delimited

class ShiftResetMonadicTests extends munit.FunSuite {

  import ShiftResetMonadic.*

  import scala.language.implicitConversions
  implicit def symbolToKCFLAE(symbol: String): Id = Id(symbol)
  implicit def intToKCFLAE(n: Int): Num           = Num(n)

  // test("all"):
  // assertEquals(
  //   interp(Let("x", 3, Fun("y", Add("x", "y")))),
  //   Closure("y", Add("x", "y"), Map("x" -> Num(3)))
  // )

  test("letadd"):
      assertEquals(
        interp(Add(4, 5)),
        Num(9)
      )

  test("letfun"):
      assertEquals(
        interp(Let("inc", Fun("x", Add("x", 1)), Add(App("inc", 4), App("inc", 5)))),
        Num(11)
      )

  // test("inc"):
  // assertEquals(
  //   interp(Let("inc", Fun("x", Add("x", 1)), "inc")),
  //   Closure("x", Add("x", 1), Map())
  // )

  test("funapp"):
      assertEquals(interp(Let("x", 3, App(Fun("y", Add("x", "y")), 4))), Num(7))

  test("funapp2"):
      assertEquals(
        interp(Let("x", 2, App(Let("x", 5, Fun("x", Add("x", "x"))), "x"))),
        Num(4)
      )

  test("resetshift0"):
      assertEquals(interp(Reset(Add(1, Shift("c", 0)))), Num(0))

  test("resetshift1"):
      assertEquals(interp(Reset(Add(1, Shift("c", App("c", 0))))), Num(1))

  test("resetshift2"):
      assertEquals(interp(Reset(Add(1, Shift("c", App("c", App("c", 0)))))), Num(2))

  test("resetshift3"):
      assertEquals(interp(Reset(Add(1, Shift("c", App("c", App("c", App("c", 0))))))), Num(3))

  test("resetshift7"):
      assertEquals(
        interp(Reset(Add(1, Shift("c", App("c", App("c", App("c", App("c", App("c", App("c", App("c", 0))))))))))),
        Num(7)
      )

  test("resetshiftif0"):
      assertEquals(
        interp(
          Let("f", Reset(If0(Shift("c", "c"), Num(2), Num(3))), Add(App("f", 0), App("f", 1)))
        ),
        Num(5)
      )
}
