package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class BooleanExpressionParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing valid BExpressions") {
    assertEquals(parser.parse("true"), Success(Position(True, (1, 1), (1, 5))))
    assertEquals(parser.parse("false"), Success(Position(False, (1, 1), (1, 6))))
    assertEquals(parser.parse("! true"), Success(Position(Not(Position(True, (1, 3), (1, 7))), (1, 1), (1, 7))))
    assertEquals(
      parser.parse("& true false"),
      Success(Position(And(Position(True, (1, 3), (1, 8)), Position(False, (1, 8), (1, 13))), (1, 1), (1, 13)))
    )
    assertEquals(
      parser.parse("| true false"),
      Success(Position(Or(Position(True, (1, 3), (1, 8)), Position(False, (1, 8), (1, 13))), (1, 1), (1, 13)))
    )
    assertEquals(
      parser.parse("== true false"),
      Success(Position(Eq(Position(True, (1, 4), (1, 9)), Position(False, (1, 9), (1, 14))), (1, 1), (1, 14)))
    )
  }

  test("Parsing invalid BExpressions") {
    assert(parser.parse("true false").isFailure)
    assert(parser.parse("& true false false").isFailure)
    assert(parser.parse("| true false false").isFailure)
    assert(parser.parse("! false false").isFailure)
  }

  test("Parsing BExpressions with nested operations") {
    assertEquals(
      parser.parse("& true (! false)"),
      Success(Position(
        And(Position(True, (1, 3), (1, 8)), Position(Not(Position(False, (1, 11), (1, 16))), (1, 9), (1, 16))),
        (1, 1),
        (1, 17)
      ))
    )
    assertEquals(
      parser.parse("| true (& false true)"),
      Success(Position(
        Or(
          Position(True, (1, 3), (1, 8)),
          Position(And(Position(False, (1, 11), (1, 17)), Position(True, (1, 17), (1, 21))), (1, 9), (1, 21))
        ),
        (1, 1),
        (1, 22)
      ))
    )
    assertEquals(
      parser.parse("& true (== false true)"),
      Success(Position(
        And(
          Position(True, (1, 3), (1, 8)),
          Position(Eq(Position(False, (1, 12), (1, 18)), Position(True, (1, 18), (1, 22))), (1, 9), (1, 22))
        ),
        (1, 1),
        (1, 23)
      ))
    )
    assertEquals(
      parser.parse("! (| false true)"),
      Success(Position(
        Not(Position(Or(Position(False, (1, 6), (1, 12)), Position(True, (1, 12), (1, 16))), (1, 4), (1, 16))),
        (1, 1),
        (1, 17)
      ))
    )
  }

  test("Parsing BExpressions with whitespaces and comments") {
    assertEquals(
      parser.parse("    &    true    false "),
      Success(Position(And(Position(True, (1, 10), (1, 18)), Position(False, (1, 18), (1, 24))), (1, 5), (1, 24)))
    )
    assertEquals(
      parser.parse("& //comment \n true false"),
      Success(Position(And(Position(True, (2, 2), (2, 7)), Position(False, (2, 7), (2, 12))), (1, 1), (2, 12)))
    )
    assertEquals(
      parser.parse("& /*comment*/ true false"),
      Success(Position(And(Position(True, (1, 15), (1, 20)), Position(False, (1, 20), (1, 25))), (1, 1), (1, 25)))
    )
  }

  test("Parsing If expressions") {
    assertEquals(
      parser.parse("if true then {true} else {false}"),
      Success(Position(
        If(Position(True, (1, 4), (1, 9)), Position(True, (1, 15), (1, 19)), Position(False, (1, 27), (1, 32))),
        (1, 1),
        (1, 33)
      ))
    )
    assertEquals(
      parser.parse("if (! false) then {& true true} else {| false true}"),
      Success(Position(
        If(
          Position(Not(Position(False, (1, 7), (1, 12))), (1, 5), (1, 12)),
          Position(And(Position(True, (1, 22), (1, 27)), Position(True, (1, 27), (1, 31))), (1, 20), (1, 31)),
          Position(Or(Position(False, (1, 41), (1, 47)), Position(True, (1, 47), (1, 51))), (1, 39), (1, 51))
        ),
        (1, 1),
        (1, 52)
      ))
    )
    assertEquals(
      parser.parse("if (& (! true) false) then {| (& true false) (| false false)} else {! true}"),
      Success(Position(
        If(
          Position(
            And(Position(Not(Position(True, (1, 10), (1, 14))), (1, 8), (1, 14)), Position(False, (1, 16), (1, 21))),
            (1, 5),
            (1, 21)
          ),
          Position(
            Or(
              Position(And(Position(True, (1, 34), (1, 39)), Position(False, (1, 39), (1, 44))), (1, 32), (1, 44)),
              Position(Or(Position(False, (1, 49), (1, 55)), Position(False, (1, 55), (1, 60))), (1, 47), (1, 60))
            ),
            (1, 29),
            (1, 61)
          ),
          Position(Not(Position(True, (1, 71), (1, 75))), (1, 69), (1, 75))
        ),
        (1, 1),
        (1, 76)
      ))
    )

    assert(parser.parse("if true else false").isFailure)
    assert(parser.parse("if true then false").isFailure)
    assert(parser.parse("if true then true else").isFailure)
    assert(parser.parse("if true then true else false else true").isFailure)
    assert(parser.parse("true then true else false").isFailure)
    assert(parser.parse("if (& true) then true else false").isFailure)
  }
}
