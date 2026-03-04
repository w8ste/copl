package interpreters.basic

// Note: generally one would put tests into a different “scope”, such that the tests are not defined as
class ArithmeticExpressionTests extends munit.FunSuite {

  import ArithmeticExpressions.*

  // Definition for convenience: We can now write 2 instead of Num(2).
  // Scala implicitly converts Int to an arithmetic expression AE.
  // N.B. implicit conversions are considered bad style in Scala
  // (thus we need the explicit language import to enable them)
  // However, there is a proposal to enable their use specifically for numeric literals (such as we do here):
  // https://docs.scala-lang.org/scala3/reference/experimental/numeric-literals.html
  import scala.language.implicitConversions
  implicit def intToAE(n: Int): Expr = Num(n)

  // we need to import the definitions from the other object to make them visible here

  test("interpreting arithmetic expressions") {
    assertEquals(interp(Num(5)), 5)
    assertEquals(interp(Num(15)), 15)

    assertEquals(interp(Add(1, 2)), 3)
    assertEquals(interp(Add(5, 15)), 20)
    assertEquals(interp(Add(Add(1, 2), Add(3, 4))), 10)

    assertEquals(interp(Sub(15, 5)), 10)
    assertEquals(interp(Sub(Add(1, 2), 3)), 0)

    assertEquals(interp(Add(Num(5), Add(Num(2), Num(3)))), 10) // should be 10
    assertEquals(interp(Add(Num(5), Sub(Num(2), Num(3)))), 4)  // should be 4
    assertEquals(interp(Sub(Num(5), Sub(Num(2), Num(3)))), 6)  // should be 6
  }

}
