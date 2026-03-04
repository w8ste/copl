package interpreters.lazyeval

class CFLAELazyCachingInterpTests extends munit.FunSuite {

  import LazyEvaluation.*
  import Caching.*

  import scala.language.implicitConversions

  implicit def idToExpr(id: String): CFLAE = Id(id)
  implicit def numToExpr(n: Int): CFLAE    = Num(n)

  test("all") {

    assertEquals(interp(Let("f", App("undef", "x"), 4)), Num(4))
    assertEquals(
      interp(
        Let("x", Add(4, 5), Let("y", Add("x", "x"), Let("z", "y", Let("x", 4, "z"))))
      ),
      Num(18)
    )
    assert(interp(Let("x", 4, Fun("y", Add("x", "y")))).isInstanceOf[FClosure])
    assert(interp(Let("f", Fun("y", Add("x", "y")), "f")).isInstanceOf[FClosure])
  }
}
