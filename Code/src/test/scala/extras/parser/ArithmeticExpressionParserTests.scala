package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class ArithmeticExpressionParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing valid SExpressions") {
    assertEquals(
      parser.parse("+ 1 2"),
      Success(Position(Add(Position(Num(1), (1, 3), (1, 5)), Position(Num(2), (1, 5), (1, 6))), (1, 1), (1, 6)))
    )
    assertEquals(
      parser.parse("- 2 1"),
      Success(Position(Sub(Position(Num(2), (1, 3), (1, 5)), Position(Num(1), (1, 5), (1, 6))), (1, 1), (1, 6)))
    )
    assertEquals(
      parser.parse("* 1 2"),
      Success(Position(Mult(Position(Num(1), (1, 3), (1, 5)), Position(Num(2), (1, 5), (1, 6))), (1, 1), (1, 6)))
    )

    // TODO: Should we add parentheses support at the beginning?
  }

  // Do we test failure cases like this?
  test("Parsing invalid SExpressions") {
    assert(parser.parse("(1 2)").isFailure)
    assert(parser.parse("+ 1").isFailure)
    assert(parser.parse("(- 2)").isFailure)
    assert(parser.parse("(+ 1 2 3)").isFailure)
    assert(parser.parse("(- 2 1 0)").isFailure)
    assert(parser.parse("((+ 2 1))").isFailure)
    assert(parser.parse("(+ - 4 3 1)").isFailure)
    assert(parser.parse("(- 2 + 1 2)").isFailure)
  }

  test("Parsing SExpressions with nested operations") {
    assertEquals(
      parser.parse("+ 1 (- 2 1)"),
      Success(Position(
        Add(
          Position(Num(1), (1, 3), (1, 5)),
          Position(Sub(Position(Num(2), (1, 8), (1, 10)), Position(Num(1), (1, 10), (1, 11))), (1, 6), (1, 11))
        ),
        (1, 1),
        (1, 12)
      ))
    )
    assertEquals(
      parser.parse("+ 1 (- 2 (+ 5 2))"),
      Success(Position(
        Add(
          Position(Num(1), (1, 3), (1, 5)),
          Position(
            Sub(
              Position(Num(2), (1, 8), (1, 10)),
              Position(Add(Position(Num(5), (1, 13), (1, 15)), Position(Num(2), (1, 15), (1, 16))), (1, 11), (1, 16))
            ),
            (1, 6),
            (1, 17)
          )
        ),
        (1, 1),
        (1, 18)
      ))
    )
    assertEquals(
      parser.parse("- (+ 2 1) 1"),
      Success(Position(
        Sub(
          Position(Add(Position(Num(2), (1, 6), (1, 8)), Position(Num(1), (1, 8), (1, 9))), (1, 4), (1, 9)),
          Position(Num(1), (1, 11), (1, 12))
        ),
        (1, 1),
        (1, 12)
      ))
    )
    assertEquals(
      parser.parse("- (+ 2 1) (+ 1 2)"),
      Success(Position(
        Sub(
          Position(Add(Position(Num(2), (1, 6), (1, 8)), Position(Num(1), (1, 8), (1, 9))), (1, 4), (1, 9)),
          Position(Add(Position(Num(1), (1, 14), (1, 16)), Position(Num(2), (1, 16), (1, 17))), (1, 12), (1, 17))
        ),
        (1, 1),
        (1, 18)
      ))
    )
  }

  test("Parsing SExpressions with multiple nested operations") {
    assertEquals(
      parser.parse("+ (- 2 1) (+ 1 2)"),
      Success(Position(
        Add(
          Position(Sub(Position(Num(2), (1, 6), (1, 8)), Position(Num(1), (1, 8), (1, 9))), (1, 4), (1, 9)),
          Position(Add(Position(Num(1), (1, 14), (1, 16)), Position(Num(2), (1, 16), (1, 17))), (1, 12), (1, 17))
        ),
        (1, 1),
        (1, 18)
      ))
    )
    assertEquals(
      parser.parse("- (+ 2 1) (+ 1 2)"),
      Success(Position(
        Sub(
          Position(Add(Position(Num(2), (1, 6), (1, 8)), Position(Num(1), (1, 8), (1, 9))), (1, 4), (1, 9)),
          Position(Add(Position(Num(1), (1, 14), (1, 16)), Position(Num(2), (1, 16), (1, 17))), (1, 12), (1, 17))
        ),
        (1, 1),
        (1, 18)
      ))
    )
    assertEquals(
      parser.parse("- (+ 2 (+ 5 2)) (+ (- 2 5) 2)"),
      Success(Position(
        Sub(
          Position(
            Add(
              Position(Num(2), (1, 6), (1, 8)),
              Position(Add(Position(Num(5), (1, 11), (1, 13)), Position(Num(2), (1, 13), (1, 14))), (1, 9), (1, 14))
            ),
            (1, 4),
            (1, 15)
          ),
          Position(
            Add(
              Position(Sub(Position(Num(2), (1, 23), (1, 25)), Position(Num(5), (1, 25), (1, 26))), (1, 21), (1, 26)),
              Position(Num(2), (1, 28), (1, 29))
            ),
            (1, 18),
            (1, 29)
          )
        ),
        (1, 1),
        (1, 30)
      ))
    )
  }

  test("Parsing SExpressions with whitespaces and comments") {
    assertEquals(
      parser.parse("   +   1   2   "),
      Success(Position(Add(Position(Num(1), (1, 8), (1, 12)), Position(Num(2), (1, 12), (1, 16))), (1, 4), (1, 16)))
    )
    assertEquals(
      parser.parse("- //comment \n 2 1"),
      Success(Position(Sub(Position(Num(2), (2, 2), (2, 4)), Position(Num(1), (2, 4), (2, 5))), (1, 1), (2, 5)))
    )
    assertEquals(
      parser.parse("- /*comment*/ 2 1"),
      Success(Position(Sub(Position(Num(2), (1, 15), (1, 17)), Position(Num(1), (1, 17), (1, 18))), (1, 1), (1, 18)))
    )
  }

  test("Parsing If0 expressions") {
    assertEquals(
      parser.parse("if0 (1) then {2} else {3}"),
      Success(Position(
        If0(Position(Num(1), (1, 6), (1, 7)), Position(Num(2), (1, 15), (1, 16)), Position(Num(3), (1, 24), (1, 25))),
        (1, 1),
        (1, 26)
      ))
    )
    assertEquals(
      parser.parse("if0 (- 1 1) then {2} else {if0 (+ 3 4) then {4} else {5}}"),
      Success(Position(
        If0(
          Position(Sub(Position(Num(1), (1, 8), (1, 10)), Position(Num(1), (1, 10), (1, 11))), (1, 6), (1, 11)),
          Position(Num(2), (1, 19), (1, 20)),
          Position(
            If0(
              Position(Add(Position(Num(3), (1, 35), (1, 37)), Position(Num(4), (1, 37), (1, 38))), (1, 33), (1, 38)),
              Position(Num(4), (1, 46), (1, 47)),
              Position(Num(5), (1, 55), (1, 56))
            ),
            (1, 28),
            (1, 57)
          )
        ),
        (1, 1),
        (1, 58)
      ))
    )
  }
}
