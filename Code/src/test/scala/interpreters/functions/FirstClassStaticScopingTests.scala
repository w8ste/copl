package interpreters.functions

class FirstClassStaticScopingTests extends munit.FunSuite {

  import FirstClassFunctions.*
  import StaticScoping.*

  import scala.language.implicitConversions

  implicit def symbolToFLAE(symbol: String): Id = Id(symbol)
  implicit def intToFLAE(n: Int): Num           = Num(n)

  test("basic") {

    assertEquals(
      interp(
        Let("x", 3, Fun("y", Add("x", "y"))),
        Map.empty
      ),
      VClosure("y", Add("x", "y"), Map("x" -> Num(3)))
    )

    assertEquals(
      interp(
        Let("inc", Fun("x", Add("x", 1)), Add(App("inc", 4), App("inc", 5))),
        Map.empty
      ),
      Num(11)
    )

    assertEquals(
      interp(
        Let("inc", Fun("x", Add("x", 1)), "inc"),
        Map.empty
      ),
      VClosure("x", Add("x", 1), Map())
    )

    assertEquals(interp(Let("x", 3, App(Fun("y", Add("x", "y")), 4)), Map.empty), Num(7))
    assertEquals(
      interp(
        Let("x", 2, App(Let("x", 5, Fun("x", Add("x", "x"))), "x")),
        Map.empty
      ),
      Num(4)
    )
  }
}
