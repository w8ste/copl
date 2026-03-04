package interpreters.recursion

class MetaClosuresTests extends munit.FunSuite {

  import Recursion.*
  import Meta.Closures.*

  import scala.language.implicitConversions

  implicit def idToRCFLAE(id: String): RCFLAE = Id(id)
  implicit def numToRCFLAE(n: Int): RCFLAE    = Num(n)

  test("unnamed") {

    assertEquals(
      interp(
        LetRec("fact", Fun("n", If0("n", 1, Mult("n", App("fact", Sub("n", 1))))), App("fact", 5))
      ),
      Num(120)
    )
    assertEquals(
      interp(
        Let("inc", Fun("x", Add("x", 1)), Add(App("inc", 4), App("inc", 5)))
      ),
      Num(11)
    )
    assertEquals(interp(Let("x", 3, App(Fun("y", Add("x", "y")), 4))), Num(7))

  }
}
