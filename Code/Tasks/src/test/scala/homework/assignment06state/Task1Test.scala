package homework.assignment06state

import homework.assignment06state.Task1EvalOrder.*
import munit.FunSuite

class Task1Test extends FunSuite {

  import scala.language.implicitConversions

  implicit def idToSCFLAE(id: String): SCFLAE = Id(id)
  implicit def numToSCFLAE(n: Int): SCFLAE    = Num(n)

  test("simple add") {
    val (result, _) = interp(Sub(61, 19))
    assertEquals(result, Num(42))
  }

  test("simple nested add") {
    val (result, _) = interp(Sub(37, Sub(7, 12)))
    assertEquals(result, Num(42))
  }

  test("state change in rhs") {
    val (result, _) = interp(Let("b", NewBox(21), Sub(OpenBox("b"), Seqn(SetBox("b", 44), 2))))
    assertEquals(result, Num(42))
  }

  test("nested state change in rhs") {
    val (result, _) = interp(
      Let(
        "b",
        NewBox(0),
        Sub(
          OpenBox("b"),
          Sub(
            Seqn(
              SetBox("b", 112),
              OpenBox("b")
            ),
            Seqn(
              SetBox("b", 42),
              OpenBox("b")
            )
          )
        )
      )
    )

    assertEquals(result, Num(42))
  }
}
