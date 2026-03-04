package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class RecordExpressionParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing Record expressions") {
    assertEquals(
      parser.parse("{x = 3, y = 2, z = 5}"),
      Success(Position(
        Record(Map(
          "x" -> Position(Num(3), (1, 6), (1, 7)),
          "y" -> Position(Num(2), (1, 13), (1, 14)),
          "z" -> Position(Num(5), (1, 20), (1, 21))
        )),
        (1, 1),
        (1, 22)
      ))
    )
  }

  test("Parsing Record projection expressions") {
    assertEquals(
      parser.parse("record {x = 3, y = 2, z = 5} x"),
      Success(Position(
        RecordProjection(
          Position(
            Record(Map(
              "x" -> Position(Num(3), (1, 13), (1, 14)),
              "y" -> Position(Num(2), (1, 20), (1, 21)),
              "z" -> Position(Num(5), (1, 27), (1, 28))
            )),
            (1, 8),
            (1, 30)
          ),
          "x"
        ),
        (1, 1),
        (1, 31)
      ))
    )
  }
}
