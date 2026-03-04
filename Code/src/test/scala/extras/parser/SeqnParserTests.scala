package extras.parser

import extras.langserver.parser.Position
import parsley.Success

class SeqnParserTests extends munit.FunSuite {
  import extras.langserver.parser.ExpressionParser.*
  import modularized.stateful.GarbageCollected.*

  test("Parsing Seqn expressions") {
    assertEquals(
      parser.parse("seqn 1; 2; 3"),
      Success(Position(
        Seqn(List(
          Position(Num(1), (1, 6), (1, 7)),
          Position(Num(2), (1, 9), (1, 10)),
          Position(Num(3), (1, 12), (1, 13))
        ).map(_.asInstanceOf[Expr])*),
        (1, 1),
        (1, 13)
      ))
    )
  }
}
