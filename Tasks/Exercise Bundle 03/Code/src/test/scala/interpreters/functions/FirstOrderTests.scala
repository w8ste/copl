package interpreters.functions

import interpreters.functions.FirstOrderFunctions.*

/** We define a common interface for the interpreters, so we can abstract over them in the tests.
  * Note that the interpretets don’t extend this interface, instead we use a feature called “Single Abstract Method” (SAM),
  * which allows us to generate and implementation of this interface by just passing a function (i.e., `interp`)
  * with the correct signature.
  * The [[@FunctionalInterface]] annotation makes it explicit that we want this trait to participate in SAM conversions.
  */
@FunctionalInterface
trait HasInterp {
  def apply(expr: F1LAE, funDefs: Map[String, FunDef]): Int
}

class FirstOrderSubstitutionTests extends FirstOrderFunctionsTests(Substitution.interp, isDynamicallyScoped = false)
class FirstOrderDynamicTests      extends FirstOrderFunctionsTests(
      Environments(dynamic = true).interp(_, _, Map.empty),
      isDynamicallyScoped = true
    )
class FirstOrderStaticTests extends FirstOrderFunctionsTests(
      Environments(dynamic = false).interp(_, _, Map.empty),
      isDynamicallyScoped = false
    )

// This test suite is parametric over the specific interp function.
// It cannot be executed directly, but instead requires to be extended by a class providing the parameter.
abstract class FirstOrderFunctionsTests(interp: HasInterp, isDynamicallyScoped: Boolean)
    extends munit.FunSuite {

  import scala.language.implicitConversions
  implicit def symbolToF1LAE(symbol: String): Id = Id(symbol)
  implicit def intToF1LAE(n: Int): Num           = Num(n)

  test("good example 1") {

    val funDefs = Map(
      "f" -> FunDef("n", App("g", Add("n", 5))),
      "g" -> FunDef("n", Sub("n", 1))
    )

    assertEquals(interp(App("f", 5), funDefs), 9)
  }

  test("good example 2") {

    val funDefs2 = Map(
      "f" -> FunDef("y", Sub("y", 1)),
      "g" -> FunDef("y", Sub("y", 1)),
      "f" -> FunDef("x", App("g", Add("x", 3)))
    )

    assertEquals(interp(App("f", 10), funDefs2), 12)

  }

  test("empty environment") {
    assertEquals(
      interp(
        Let("x", 3, Add(Let("x", 4, Add("x", 3)), "x")),
        Map(),
      ),
      10
    )
  }

  test("dynamic scoping example") {
    def testcase() = interp(Let("n", 5, App("f", 10)), Map("f" -> FunDef("x", "n")))

    if isDynamicallyScoped
    then assertEquals(testcase(), 5)
    else intercept[Exception](testcase())
  }

  test("bad example 2") {
    intercept[Exception] {
      interp(App("f", 4), Map("f" -> FunDef("y", Add("x", "y"))))
    }
  }
}
