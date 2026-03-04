package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class FunctionExpressionParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing Id expressions") {
    assertEquals(parser.parse("x"), Success(Position(Id("x"), (1, 1), (1, 2))))

    // this fails because in the lexer, we specify what keywords don't count as identifiers
    assert(parser.parse("let").isFailure)
  }

  test("Parsing function definitions") {
    assertEquals(
      parser.parse("def x {+ x 2}"),
      Success(Position(
        Fun(
          "x",
          Position(Add(Position(Id("x"), (1, 10), (1, 12)), Position(Num(2), (1, 12), (1, 13))), (1, 8), (1, 13))
        ),
        (1, 1),
        (1, 14)
      ))
    )
    assertEquals(
      parser.parse("def x {- (+ x x) (+ 2 3)}"),
      Success(Position(
        Fun(
          "x",
          Position(
            Sub(
              Position(Add(Position(Id("x"), (1, 13), (1, 15)), Position(Id("x"), (1, 15), (1, 16))), (1, 11), (1, 16)),
              Position(Add(Position(Num(2), (1, 21), (1, 23)), Position(Num(3), (1, 23), (1, 24))), (1, 19), (1, 24))
            ),
            (1, 8),
            (1, 25)
          )
        ),
        (1, 1),
        (1, 26)
      ))
    )
    assertEquals(
      parser.parse("def x {x}"),
      Success(Position(Fun("x", Position(Id("x"), (1, 8), (1, 9))), (1, 1), (1, 10)))
    )

    assert(parser.parse("def x (+ x 2) (+ x 2)").isFailure)
    assert(parser.parse("def x (+ x)").isFailure)
  }

  test("Parsing function applications") {
    // TODO: Fix this test
    assertEquals(
      parser.parse("app {def x {+ x 2}} with {2}"),
      Success(Position(
        App(
          Position(
            Fun(
              "x",
              Position(Add(Position(Id("x"), (1, 15), (1, 17)), Position(Num(2), (1, 17), (1, 18))), (1, 13), (1, 18))
            ),
            (1, 6),
            (1, 19)
          ),
          Position(Num(2), (1, 27), (1, 28))
        ),
        (1, 1),
        (1, 29)
      ))
    )
  }
}
