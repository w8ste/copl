package interpreters.lazyeval

class CFLAEAlmostStrictTests extends munit.FunSuite {

  import LazyEvaluation.*
  import AlmostStrict.*

  import scala.language.implicitConversions

  implicit def idToExpr(id: String): CFLAE = Id(id)
  implicit def numToExpr(n: Int): CFLAE    = Num(n)

  test("all") {

    assertEquals(
      interp(Let("x", 3, Fun("y", Add("x", "y")))),
      FClosure("y", Add("x", "y"), Map("x" -> EClosure(3, Map())))
    )
    assertEquals(interp(Let("x", 10, Add("x", "x"))), Num(20))
    assertEquals(
      interp(Let("inc", Fun("x", Add("x", 1)), Add(App("inc", 4), App("inc", 5)))),
      Num(11)
    )
    assertEquals(interp(Let("x", 3, App(Fun("y", Add("x", "y")), 4))), Num(7))
    assertEquals(interp(Let("f", App("undef", "x"), 4)), Num(4))
    assertEquals(interp(App(Fun("x", 3), "y")), Num(3))
    assertEquals(interp(If0(Add(4, -4), App(Fun("x", 3), "y"), 8)), Num(3))
    assert(interp(
      Let("x", Add(4, 5), Let("y", Add("x", "x"), Let("z", "y", Let("x", 4, "z"))))
    ).isInstanceOf[EClosure])
  }
}
