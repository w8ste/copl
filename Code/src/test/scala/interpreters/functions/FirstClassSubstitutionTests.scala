package interpreters.functions

class FirstClassSubstitutionTests extends munit.FunSuite {

  import FirstClassFunctions.*
  import Substitution.*

  import scala.language.implicitConversions

  implicit def symbolToFLAE(symbol: String): Id = Id(symbol)
  implicit def intToFLAE(n: Int): Num           = Num(n)

  test("basic assertions") {
    assertEquals(
      interp(
        Let("x", 3, Fun("y", Add("x", "y"))),
        ()
      ),
      Fun("y", Add(Num(3), Id("y")))
    )
    assertEquals(
      interp(
        Let("inc", Fun("x", Add("x", 1)), Add(App("inc", 4), App("inc", 5))),
        ()
      ),
      Num(11)
    )
    assertEquals(interp(App(Let("x", 3, Fun("y", Add("x", "y"))), Add(4, 5)), ()), Num(12))
  }
}
