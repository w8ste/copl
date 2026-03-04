package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class SetIdParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing valid SetId") {
    assertEquals(
      parser.parse("set x = 1"),
      Success(Position(SetId("x", Position(Num(1), (1, 9), (1, 10))), (1, 1), (1, 10)))
    )
    assertEquals(
      parser.parse("set y = 2"),
      Success(Position(SetId("y", Position(Num(2), (1, 9), (1, 10))), (1, 1), (1, 10)))
    )
    assertEquals(
      parser.parse("set z = 3"),
      Success(Position(SetId("z", Position(Num(3), (1, 9), (1, 10))), (1, 1), (1, 10)))
    )
  }
}
