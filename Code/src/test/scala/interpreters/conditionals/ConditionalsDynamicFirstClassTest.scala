package interpreters.conditionals

class ConditionalsDynamicFirstClassTest extends munit.FunSuite {

  import ConditionalsDynamicFirstClass.*

  import scala.language.implicitConversions

  implicit def symbolToCFLAE(symbol: String): CFLAE = Id(symbol)
  implicit def intToCFLAE(n: Int): CFLAE            = Num(n)

  test("conditions tests") {

    val res = interp(
      Let("fact", Fun("n", If0("n", 1, Mult("n", App("fact", Sub("n", 1))))), App("fact", 5))
    )
    res match {
      case Num(n) => println("fact 5 = " + n)
      case _      => sys.error("interpretation should return a number, but got: " + res)
    }

    assertEquals(
      interp(
        Let("x", 3, Let("f", Fun("y", Add("x", "y")), Let("x", 5, App("f", 4))))
      ),
      Num(9)
    )

    assertEquals(
      interp(
        Let("x", 3, Fun("y", Add("x", "y")))
      ),
      Fun("y", Add("x", "y"))
    )

    assertEquals(
      interp(
        Let("inc", Fun("x", Add("x", 1)), Add(App("inc", 4), App("inc", 5)))
      ),
      Num(11)
    )

    assertEquals(
      interp(
        Let("x", 2, App(Let("x", 5, Fun("x", Add("x", "x"))), "x"))
      ),
      Num(4)
    )

  }
}
