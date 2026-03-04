package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class SymParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing valid symbols") {
    assertEquals(parser.parse("x"), Success(Position(Id("x"), (1, 1), (1, 2))))
  }
}
