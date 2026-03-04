package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class TypedFunParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing valid TypedFun") {
    assertEquals(
      parser.parse("fun (x: TNum) {x}"),
      Success(Position(TypedFun("x", TNum(), Position(Id("x"), (1, 16), (1, 17))), (1, 1), (1, 18)))
    )
    assertEquals(
      parser.parse("fun (x: TBool) {x}"),
      Success(Position(TypedFun("x", TBool, Position(Id("x"), (1, 17), (1, 18))), (1, 1), (1, 19)))
    )
    assertEquals(
      parser.parse("fun (x: TFun) {x}"),
      Success(Position(
        TypedFun("x", TFun(TUnspecified, TUnspecified), Position(Id("x"), (1, 16), (1, 17))),
        (1, 1),
        (1, 18)
      ))
    )
    assertEquals(
      parser.parse("fun (x: TRecord) {x}"),
      Success(Position(TypedFun("x", TRecord(Map.empty), Position(Id("x"), (1, 19), (1, 20))), (1, 1), (1, 21)))
    )
    assertEquals(
      parser.parse("fun (x: TUnspecified) {x}"),
      Success(Position(TypedFun("x", TUnspecified, Position(Id("x"), (1, 24), (1, 25))), (1, 1), (1, 26)))
    )
    assertEquals(
      parser.parse("fun (x: Error) {x}"),
      Success(Position(TypedFun("x", Error(""), Position(Id("x"), (1, 17), (1, 18))), (1, 1), (1, 19)))
    )
  }

  test("Parsing invalid TypedFun") {
    try
      parser.parse("fun (x: Blub) {x}")
    catch {
      case e: IllegalArgumentException => assertEquals(e.getMessage, "Unknown type: Blub")
    }
  }
}
