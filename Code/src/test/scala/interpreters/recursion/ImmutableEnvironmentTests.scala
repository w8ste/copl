package interpreters.recursion

class ImmutableEnvironmentTests extends munit.FunSuite {

  import Recursion.*
  import ImmutableEnvironment.*

  import scala.language.implicitConversions
  implicit def idToExpr(id: String): Id = Id(id)
  implicit def numToExpr(n: Int): Num   = Num(n)

  test("all") {

    assertEquals(
      interp(
        LetRec("fact", Fun("n", If0("n", 1, Mult("n", App("fact", Sub("n", 1))))), App("fact", 5))
      ),
      Num(120)
    )
    {
      val res = interp(
        Let("x", 3, Fun("y", Add("x", "y")))
      )
      assert {
        res match
            case Closure("y", Add(Id("x"), Id("y")), env: Map[String, Box[Value]]) =>
              assertEquals(env("x").value, Num(3))
              true
            case other => false
      }

    }
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
