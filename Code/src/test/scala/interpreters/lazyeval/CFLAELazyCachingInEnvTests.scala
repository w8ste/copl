package interpreters.lazyeval

class CFLAELazyCachingInEnvTests extends munit.FunSuite {
  import LazyEvaluation.*
  import CachingInEnv.*

  import scala.language.implicitConversions

  implicit def idToExpr(id: String): CFLAE = Id(id)
  implicit def numToExpr(n: Int): CFLAE    = Num(n)

  test("all") {

    assertEquals(interp(Let("f", App("undef", "x"), 4)).value, NumV(4))
    assertEquals(
      interp(
        Let("x", Add(4, 5), Let("y", Add("x", "x"), Let("z", "y", Let("x", 4, "z"))))
      ).value,
      NumV(18)
    )
    assert(interp(Let("x", 4, Fun("y", Add("x", "y")))).value.isInstanceOf[FClosure])
    assert(interp(Let("f", Fun("y", Add("x", "y")), "f")).value.isInstanceOf[FClosure])

    assertEquals(
      interp(
        Let("x", Add(4, 5), Let("y", Add("x", "x"), Let("z", "y", Let("x", 4, "z"))))
      ),
      Result(Map("x" -> NumV(9), "y" -> NumV(18), "z" -> NumV(18)), NumV(18))
    )
  }
}
