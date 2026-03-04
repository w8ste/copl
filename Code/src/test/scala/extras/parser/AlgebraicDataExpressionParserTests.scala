package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class AlgebraicDataExpressionParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing valid AlgebraicDataExpressions") {
    assertEquals(
      parser.parse("data x { 1, 2 }"),
      Success(Position(
        Data("x", List(Position(Num(1), (1, 10), (1, 11)), Position(Num(2), (1, 13), (1, 15)))),
        (1, 1),
        (1, 16)
      ))
    )
    assertEquals(
      parser.parse("data y { 1, true, 3 }"),
      Success(Position(
        Data(
          "y",
          List(Position(Num(1), (1, 10), (1, 11)), Position(True, (1, 13), (1, 17)), Position(Num(3), (1, 19), (1, 21)))
        ),
        (1, 1),
        (1, 22)
      ))
    )
    assertEquals(
      parser.parse("data z { 1, false, 3, x }"),
      Success(Position(
        Data(
          "z",
          List(
            Position(Num(1), (1, 10), (1, 11)),
            Position(False, (1, 13), (1, 18)),
            Position(Num(3), (1, 20), (1, 21)),
            Position(Id("x"), (1, 23), (1, 25))
          )
        ),
        (1, 1),
        (1, 26)
      ))
    )
  }
}
