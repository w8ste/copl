package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class LetRecParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing Let recursive expressions") {
    assertEquals(
      parser.parse("let-rec x = 2 in {x}"),
      Success(Position(
        LetRec("x", Position(Num(2), (1, 13), (1, 15)), Position(Id("x"), (1, 19), (1, 20))),
        (1, 1),
        (1, 21)
      ))
    )
  }
}
