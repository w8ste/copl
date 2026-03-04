package interpreters.recursion

class SubstitutionTests extends munit.FunSuite {

  import Recursion.*
  import Substitution.*

  import scala.language.implicitConversions

  implicit def symbolToRCFLAE(symbol: String): Id = Id(symbol)
  implicit def intToRCFLAE(n: Int): Num           = Num(n)

  test("unnamed") {

    assertEquals(
      interp(
        Let("x", 3, Fun("y", Add("x", "y")))
      ),
      Fun("y", Add(Num(3), Id("y")))
    )

    assertEquals(
      interp(
        Let("inc", Fun("x", Add("x", 1)), Mult(App("inc", 4), App("inc", 5)))
      ),
      Num(30)
    )

    val res = interp(
      Let("inc", Fun("x", Add("x", 1)), Mult(App("inc", 4), App("inc", 5)))
    )

    res match {
      case Num(n) => println(n)
      case _      => sys.error("inpterp should return a number, but returned " + res)
    }

    val factRes = interp(
      LetRec("fact", Fun("n", If0("n", 1, Mult("n", App("fact", Sub("n", 1))))), App("fact", 5))
    )

    factRes match {
      case Num(n) => println("fact 5 = " + n)
      case _      => sys.error("interpretation should return a number, but got: " + Num)
    }

    assertEquals(interp(App(Let("x", 3, Fun("y", Add("x", "y"))), Add(4, 5))), Num(12))
  }
}
