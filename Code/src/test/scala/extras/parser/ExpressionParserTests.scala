package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class ExpressionParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("mixed expressions") {
    val result1 = parser.parse("let x = 5 in {if (! false) then {+ x 3} else {* x x}}")
    assertEquals(
      result1,
      Success(Position(
        Let(
          "x",
          Position(Num(5), (1, 9), (1, 11)),
          Position(
            If(
              Position(Not(Position(False, (1, 21), (1, 26))), (1, 19), (1, 26)),
              Position(Add(Position(Id("x"), (1, 36), (1, 38)), Position(Num(3), (1, 38), (1, 39))), (1, 34), (1, 39)),
              Position(Mult(Position(Id("x"), (1, 49), (1, 51)), Position(Id("x"), (1, 51), (1, 52))), (1, 47), (1, 52))
            ),
            (1, 15),
            (1, 53)
          )
        ),
        (1, 1),
        (1, 54)
      ))
    )

    result1 match {
      case Success(v) =>
        interp(v) match {
          case Num(n) => assertEquals(n, 8)
          case _      => println(interp(v)); fail("Expected Num")
        }
      case _ => fail("Expected Let expression")
    }

    val result2 = parser.parse("if0 (- 5 (+ 2 3)) then {if ! true then {1} else {2}} else {3}")
    assertEquals(
      result2,
      Success(Position(
        If0(
          Position(
            Sub(
              Position(Num(5), (1, 8), (1, 10)),
              Position(Add(Position(Num(2), (1, 13), (1, 15)), Position(Num(3), (1, 15), (1, 16))), (1, 11), (1, 16))
            ),
            (1, 6),
            (1, 17)
          ),
          Position(
            If(
              Position(Not(Position(True, (1, 30), (1, 35))), (1, 28), (1, 35)),
              Position(Num(1), (1, 41), (1, 42)),
              Position(Num(2), (1, 50), (1, 51))
            ),
            (1, 25),
            (1, 52)
          ),
          Position(Num(3), (1, 60), (1, 61))
        ),
        (1, 1),
        (1, 62)
      ))
    )

    result2 match {
      case Success(v) =>
        interp(v) match {
          case Num(n) => assertEquals(n, 2)
          case _      => println(interp(v)); fail("Expected True")
        }
      case _ => fail("Expected If0 expression")
    }

    val result3 = parser.parse("app {def x {* x x}} with {let x = 5 in {* (if0 (- 2 4) then {2} else {x}) x}}")
    assertEquals(
      result3,
      Success(Position(
        App(
          Position(
            Fun(
              "x",
              Position(Mult(Position(Id("x"), (1, 15), (1, 17)), Position(Id("x"), (1, 17), (1, 18))), (1, 13), (1, 18))
            ),
            (1, 6),
            (1, 19)
          ),
          Position(
            Let(
              "x",
              Position(Num(5), (1, 35), (1, 37)),
              Position(
                Mult(
                  Position(
                    If0(
                      Position(
                        Sub(Position(Num(2), (1, 51), (1, 53)), Position(Num(4), (1, 53), (1, 54))),
                        (1, 49),
                        (1, 54)
                      ),
                      Position(Num(2), (1, 62), (1, 63)),
                      Position(Id("x"), (1, 71), (1, 72))
                    ),
                    (1, 44),
                    (1, 73)
                  ),
                  Position(Id("x"), (1, 75), (1, 76))
                ),
                (1, 41),
                (1, 76)
              )
            ),
            (1, 27),
            (1, 77)
          )
        ),
        (1, 1),
        (1, 78)
      ))
    )

    result3 match {
      case Success(v) =>
        interp(v) match {
          case Num(n) => assertEquals(n, 625)
          case _      => println(interp(v)); fail("Expected Num")
        }
      case _ => fail("Expected App expression")
    }
  }
}
