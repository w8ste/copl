package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class LetParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing Let expressions") {
    assertEquals(
      parser.parse("let x = 2 in {x}"),
      Success(Position(
        Let("x", Position(Num(2), (1, 9), (1, 11)), Position(Id("x"), (1, 15), (1, 16))),
        (1, 1),
        (1, 17)
      ))
    )
    assertEquals(
      parser.parse("let x = 2 in {+ x 3}"),
      Success(Position(
        Let(
          "x",
          Position(Num(2), (1, 9), (1, 11)),
          Position(Add(Position(Id("x"), (1, 17), (1, 19)), Position(Num(3), (1, 19), (1, 20))), (1, 15), (1, 20))
        ),
        (1, 1),
        (1, 21)
      ))
    )
    assertEquals(
      parser.parse("let x = 2 in {let y = 3 in {+ x y}}"),
      Success(Position(
        Let(
          "x",
          Position(Num(2), (1, 9), (1, 11)),
          Position(
            Let(
              "y",
              Position(Num(3), (1, 23), (1, 25)),
              Position(Add(Position(Id("x"), (1, 31), (1, 33)), Position(Id("y"), (1, 33), (1, 34))), (1, 29), (1, 34))
            ),
            (1, 15),
            (1, 35)
          )
        ),
        (1, 1),
        (1, 36)
      ))
    )
    assertEquals(
      parser.parse("let x = let y = 3 in {+ x y} in {+ x y}"),
      Success(Position(
        Let(
          "x",
          Position(
            Let(
              "y",
              Position(Num(3), (1, 17), (1, 19)),
              Position(Add(Position(Id("x"), (1, 25), (1, 27)), Position(Id("y"), (1, 27), (1, 28))), (1, 23), (1, 28))
            ),
            (1, 9),
            (1, 30)
          ),
          Position(Add(Position(Id("x"), (1, 36), (1, 38)), Position(Id("y"), (1, 38), (1, 39))), (1, 34), (1, 39))
        ),
        (1, 1),
        (1, 40)
      ))
    )

    assert(parser.parse("let x = 2 in {x = 3}").isFailure)
    assert(parser.parse("let x = 2 in {(+ x)}").isFailure)
    assert(parser.parse("let (+ 2 4) = 2 in {3}").isFailure)
    assert(parser.parse("let let 2 in {3}").isFailure)
    assert(parser.parse("let x = 2 in let y = 3 in {(+ x)}").isFailure)
  }
}
