package interpreters.recursion

class EnvironmentTests extends munit.FunSuite {

  import Recursion.*
  import Environment.*

  import scala.language.implicitConversions
  implicit def idToExpr(id: String): Id                   = Id(id)
  implicit def numToExpr(n: Int): Num                     = Num(n)
  implicit def valueToBox(v: Value): Box.Immutable[Value] = Box.Immutable[Value](v)

  test("all") {

    assertEquals(
      interp(
        LetRec("fact", Fun("n", If0("n", 1, Mult("n", App("fact", Sub("n", 1))))), App("fact", 5))
      ),
      Num(120)
    )
    assertEquals(
      interp(
        Let("x", 3, Fun("y", Add("x", "y")))
      ),
      Closure("y", Add("x", "y"), Map("x" -> Num(3)))
    )
    assertEquals(
      interp(
        Let("inc", Fun("x", Add("x", 1)), Add(App("inc", 4), App("inc", 5)))
      ),
      Num(11)
    )
    assertEquals(
      interp(
        Let("inc", Fun("x", Add("x", 1)), "inc")
      ),
      Closure("x", Add("x", 1), Map())
    )
    assertEquals(interp(Let("x", 3, App(Fun("y", Add("x", "y")), 4))), Num(7))

    assertEquals(
      interp(
        LetRec("fact", App(Fun("x", "x"), Fun("n", If0("n", 1, Mult("n", App("fact", Sub("n", 1)))))), App("fact", 5))
      ),
      Num(120)
    )

  }
}
