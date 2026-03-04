package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class BoxExpressionParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing valid BoxExpressions") {
    assertEquals(
      parser.parse("box-new {1}"),
      Success(Position(NewBox(Position(Num(1), (1, 10), (1, 11))), (1, 1), (1, 12)))
    )
    assertEquals(
      parser.parse("box-set {box-new {1}} = {2}"),
      Success(Position(
        SetBox(
          Position(NewBox(Position(Num(1), (1, 19), (1, 20))), (1, 10), (1, 21)),
          Position(Num(2), (1, 26), (1, 27))
        ),
        (1, 1),
        (1, 28)
      ))
    )
    assertEquals(
      parser.parse("box-open {box-new {1}}"),
      Success(Position(
        OpenBox(Position(NewBox(Position(Num(1), (1, 20), (1, 21))), (1, 11), (1, 22))),
        (1, 1),
        (1, 23)
      ))
    )
  }
}
