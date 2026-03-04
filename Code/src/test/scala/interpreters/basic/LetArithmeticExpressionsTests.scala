package interpreters.basic

class LetArithmeticExpressionsTests extends munit.FunSuite {

  import LetArithmeticExpressions.*

  /*
   * Implicit definitions for convenience. Instead of writing, e.g., Num(2) we can write
   * 2, and instead of writing, e.g., Id("x") we can write "x" instead.
   */
  import scala.language.implicitConversions
  implicit def symbolToLAE(symbol: String): Expr = Id(symbol)
  implicit def intToLAE(n: Int): Expr            = Num(n)

  // here we abstract over the two evaluation functions.
  // we put them both into a list, add a label for nicer output,
  // and use `foreach` to run the test on both functions.
  List(
    ("eager", interpEager),
    ("lazy", interpLazy)
  ).foreach { (name, interp) =>
    test(s"common test with $name") {

      assertEquals(interp(Let("x", Add(5, 5), Add("x", "x"))), 20)
      assertEquals(interp(Let("x", 5, Add("x", "x"))), 10)
      assertEquals(interp(Let("x", Add(5, 5), Let("y", Sub("x", 3), Add("y", "y")))), 14)
      assertEquals(interp(Let("x", 5, Let("y", Sub("x", 3), Add("y", "y")))), 4)
      assertEquals(interp(Let("x", 5, Add("x", Let("x", 3, 10)))), 15)
      assertEquals(interp(Let("x", 5, Add("x", Let("x", 3, "x")))), 8)
      assertEquals(interp(Let("x", 5, Add("x", Let("y", 3, "x")))), 10)
      assertEquals(interp(Let("x", 5, Let("y", "x", "y"))), 5)
      assertEquals(interp(Let("x", 5, Let("x", "x", "x"))), 5)

    }
  }

  test("eager test") {
    intercept[Exception] {
      interpEager(Let("x", Add(3, "z"), Let("y", 100, "y")))
    }
  }

  test("lazy test") {
    assertEquals(interpLazy(Let("x", Add(3, "z"), Let("y", 100, "y"))), 100)
  }

  test("variable capture") {
    val potentialCapture = Let("x", "y", Let("y", 10, "x"))
    // the eager interpreter fails as expected, because of the unbound variable "y"
    intercept[Exception](interpEager(potentialCapture))
    // the lazy interpreter however actually returns 10, which is not necessarily what we expect
    assertEquals(interpLazy(potentialCapture), 10)
    // the evaluation trace is:
    val trace = List(
      Let("x", "y", Let("y", 10, "x")),
      Let("y", 10, "y"), // substitute lazily – y is incorrectly captured
      10                 // substitute again
    )
  }

}
