package interpreters.lazyeval

class CFLAELazyTests extends munit.FunSuite {

  import LazyEvaluation.*
  import NoStrict.*

  import scala.language.implicitConversions
  implicit def symbtolToCFLAE(s: String): Id = Id(s)
  implicit def intToCFLAE(n: Int): Num       = Num(n)

  test("all") {

    intercept[RuntimeException] {
      interp(Let("x", Num(10), Add(Id("x"), Id("x"))))
    }

    assert(interp(App(Fun("x", Id("x")), Num(3))).isInstanceOf[EClosure])

    val e = Let("x", Add("y", Num(5)), Let("y", Num(3), "x"))

    intercept[NoSuchElementException](strict(interp(e)))
  }
}
