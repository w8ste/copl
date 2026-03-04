package homework.assignment06state

import homework.assignment06state.Task2N_Seqn.*
import munit.FunSuite

class Task2Test extends FunSuite {

  import scala.language.implicitConversions

  implicit def idToSCFLAE(id: String): SCFLAE = Id(id)
  implicit def numToSCFLAE(n: Int): SCFLAE    = Num(n)

  test("sample usage") {
    val (result, _) = interp(Seqn(List[SCFLAE](1, 2, 3)))
    assertEquals(result, Num(3))
  }

  // Some tests expect an exception to be thrown.
  // Calling "sys.error(...)" like in the already implemented cases throws the expected "RuntimeException".
  test("error on empty seqn") {
    intercept[Exception] { interp(Seqn(Nil)) }
  }

  test("single expression") {
    val (result, _) = interp(Seqn(List(3)))
    assertEquals(result, Num(3))
  }

  test("many simple expressions last is return value") {
    val (result, _) = interp(Seqn(List(1, 2, 3)))
    assertEquals(result, Num(3))
  }

  test("nested seqn") {
    val (result, _) = interp(Seqn(List(1, Seqn(List(2, 3)))))
    assertEquals(result, Num(3))
  }

  test("propagates side effect from inner seqn (2/3)") {
    val (result, _) = interp(
      Let(
        "b",
        NewBox(-1),
        Seqn(List[SCFLAE](
          1,
          SetBox("b", 42),
          OpenBox("b")
        ))
      )
    )
    assertEquals(result, Num(42))
  }

  test("interpuates from left to right") {
    val (result, _) = interp(
      Let(
        "b",
        NewBox(-1),
        Seqn(List[SCFLAE](
          SetBox("b", Fun("x", Add("x", 1))),
          SetBox("b", App(OpenBox("b"), 1)),
          OpenBox("b")
        ))
      )
    )
    assertEquals(result, Num(2))
  }

  test("does not interpuate last expression more than once") {
    val (result, _) = interp(
      Let(
        "b",
        NewBox(0),
        Seqn(List[SCFLAE](
          SetBox("b", Add(OpenBox("b"), 1)),
          SetBox("b", Add(OpenBox("b"), 1)),
          Seqn(List(
            SetBox("b", Add(OpenBox("b"), 1)),
            OpenBox("b")
          ))
        ))
      )
    )
    assertEquals(result, Num(3))
  }
}
