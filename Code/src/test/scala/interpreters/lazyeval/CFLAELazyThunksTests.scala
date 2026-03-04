package interpreters.lazyeval

class CFLAELazyThunksTests extends munit.FunSuite {

  import LazyEvaluation.*
  import LazyThunks.*

  import scala.language.implicitConversions

  implicit def idToExpr(id: String): Id = Id(id)
  implicit def numToExpr(n: Int): Num   = Num(n)

  test("all") {

    assert(interp(Let("x", 3, "x")) == Num(3))
    assert(interp(App(Fun("x", "x"), 3)) == Num(3))
    assert(interp(Let("x", 3, Fun("y", Add("x", "y")))).isInstanceOf[EClosure])
    assert(interp(Let("inc", Fun("x", Add("x", 1)), Add(App("inc", 4), App("inc", 5)))) == Num(11))
    assert(interp(Let("inc", Fun("x", Add("x", 1)), "inc")).isInstanceOf[EClosure])
    assert(interp(Let("x", 3, App(Fun("y", Add("x", "y")), 4))) == Num(7))
    assert(interp(Let("f", App("undef", "x"), 4)) == Num(4))
    assert(interp(Let("x", Add(4, 5), Let("y", Add("x", "x"), Let("z", "y", Let("x", 4, "z"))))) == Num(18))
    assert(interp(App(Let("x", 3, Fun("y", Add("x", "y"))), 6)) == Num(9))
  }
}
