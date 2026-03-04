package interpreters.differentiation

class DualNumbersTests extends munit.FunSuite {

  import DualNumbers.*

  import scala.language.implicitConversions

  implicit def symbolToFLAE(symbol: String): Id = Id(symbol)
  implicit def intToFLAE(n: Int): Num           = Num(n)

  test("basic") {

    assertEquals(
      interp(
        Let("x", 3, Fun("y", Add("x", "y"))),
        Map.empty
      ),
      VClosure("y", Add("x", "y"), Map("x" -> Dual(3, 0)))
    )

    assertEquals(
      interp(
        Let("inc", Fun("x", Add("x", 1)), Add(App("inc", 4), App("inc", 5))),
        Map.empty
      ),
      Dual(11, 0)
    )

    assertEquals(
      interp(
        Let("inc", Fun("x", Add("x", 1)), "inc"),
        Map.empty
      ),
      VClosure("x", Add("x", 1), Map())
    )

    assertEquals(interp(Let("x", 3, App(Fun("y", Add("x", "y")), 4)), Map.empty), Dual(7, 0))
    assertEquals(
      interp(
        Let("x", 3, App(Let("x", 5, Fun("x", Mul("x", "x"))), "x")),
        Map.empty
      ),
      Dual(9, 0)
    )

//    assertEquals(
//      interp(
//        Diff(Fun("x", Mul(Id("x"), Mul(Id("x"), Id("x")))), Num(7)),
//        Map.empty
//      ),
//      Dual(147, 0)
//    )
  }
}
