package homework.assignment04recursionfix

import homework.assignment04recursionfix.LetRecWithMultipleDefinitions.*
import munit.FunSuite

class LetRecWithMultipleDefinitionsTest extends FunSuite {

  // don’t do this at home :-)

  import scala.language.implicitConversions

  implicit def symbolToExpr(symbol: String): Id = Id(symbol)
  implicit def intToExpr(n: Int): Num           = Num(n)

  test("Simple Recursion") {
    assertEquals(
      interp(LetRec(List("fac" -> Fun("n", If0("n", 1, Mult("n", App("fac", Add("n", -1)))))), App("fac", 5))),
      Num(120)
    )
  }

  test("Mutual Recursion 1") {
    assertEquals(
      interp(LetRec(
        List(
          "odd"  -> Fun("n", If0("n", 0, App("even", Sub("n", 1)))),
          "even" -> Fun("n", If0("n", 1, App("odd", Sub("n", 1))))
        ),
        App("odd", 5)
      )),
      Num(1)
    )
  }

  test("Mutual Recursion 2") {
    assertEquals(
      interp(LetRec(
        List(
          "mod3"   -> Fun("n", If0("n", 0, App("mod3_1", Sub("n", 1)))),
          "mod3_1" -> Fun("n", If0("n", 1, App("mod3_2", Sub("n", 1)))),
          "mod3_2" -> Fun("n", If0("n", 2, App("mod3", Sub("n", 1))))
        ),
        App("mod3", 42)
      )),
      Num(0)
    )
  }

  test("Nested LetRec") {
    assertEquals(
      interp(LetRec(
        List("fac" -> Fun("n", If0("n", 1, Mult("n", App("fac", Add("n", -1)))))),
        LetRec(
          List(
            "odd"  -> Fun("n", If0("n", 0, App("even", Sub("n", 1)))),
            "even" -> Fun("n", If0("n", 1, App("odd", Sub("n", 1))))
          ),
          If0(App("odd", 7), App("fac", 12), App("fac", 7))
        )
      )),
      Num(5040)
    )
  }

  test("Shadowing") {
    assertEquals(
      interp(LetRec(
        List("a" -> Fun("x", App("sub3", "x")), "sub3" -> Fun("x", Sub("x", 3))),
        LetRec(List("sub3" -> Fun("x", Mult("x", 9)), "x" -> Fun("y", Mult(3, "y"))), App("a", App("sub3", 12)))
      )),
      Num(105)
    )
  }

  test("Just Numbers") {
    assertEquals(interp(LetRec(List("x" -> 1, "y" -> 5), "y")), Num(5))
  }

  test("Outer cannot access inner") {
    intercept[NoSuchElementException] {
      interp(LetRec(List("outer" -> Fun("x", "inner")), LetRec(List("inner" -> 12), App("outer", 0))))
    }
  }

}
