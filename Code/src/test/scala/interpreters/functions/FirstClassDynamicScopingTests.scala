package interpreters.functions

class FirstClassDynamicScopingTests extends munit.FunSuite {

  import FirstClassFunctions.*
  import DynamicScoping.*

  import scala.language.implicitConversions

  implicit def symbolToFLAE(symbol: String): FLAE = Id(symbol)
  implicit def intToFLAE(n: Int): FLAE            = Num(n)

  test("basic tests") {
    assertEquals(
      interp(
        Let("x", 3, Let("f", Fun("y", Add("x", "y")), Let("x", 5, App("f", 4)))),
        Map.empty
      ),
      Num(9)
    )

    assertEquals(
      interp(
        Let("x", 3, Fun("y", Add("x", "y"))),
        Map.empty
      ),
      Fun("y", Add("x", "y"))
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
        Let("x", 2, App(Let("x", 5, Fun("x", Add("x", "x"))), "x")),
        Map.empty
      ),
      Num(4)
    )
  }
}
