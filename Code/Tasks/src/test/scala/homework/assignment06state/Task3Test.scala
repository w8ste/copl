package homework.assignment06state

import homework.assignment06state.Task3Until0.*
import munit.FunSuite

class Task3Test extends FunSuite {

  import scala.language.implicitConversions

  implicit def idToSCFLAE(id: String): SCFLAE = Id(id)
  implicit def numToSCFLAE(n: Int): SCFLAE    = Num(n)

  test("zero iterations") {
    val (result, _) = interp(
      Let(
        "n",
        NewBox(42),
        Seqn(
          Until0(0, SetBox("n", 0)),
          OpenBox("n")
        )
      )
    )
    assertEquals(Num(42), result)
  }

  test("sum of 0..4") {
    val (result, _) = interp(
      Let(
        "n",
        NewBox(0),
        Let(
          "i",
          NewBox(4),
          Seqn(
            Until0(
              OpenBox("i"),
              Seqn(
                SetBox("n", Add(OpenBox("n"), OpenBox("i"))),
                SetBox("i", Add(OpenBox("i"), -1))
              )
            ),
            OpenBox("n")
          )
        )
      )
    )
    assertEquals(Num(10), result)
  }

  test("side effect in test") {
    val (result, _) = interp(
      Let(
        "d",
        NewBox(-2),
        Let(
          "n",
          NewBox(0),
          Let(
            "i",
            NewBox(4),
            Seqn(
              Until0(
                Seqn(
                  SetBox("d", -1),
                  OpenBox("i")
                ),
                Seqn(
                  SetBox("n", Add(OpenBox("n"), OpenBox("i"))),
                  SetBox("i", Add(OpenBox("i"), OpenBox("d")))
                )
              ),
              OpenBox("n")
            )
          )
        )
      )
    )
    assertEquals(Num(10), result)
  }

  test("does not assume decrement for test") {
    val (result, _) = interp(
      Let(
        "f",
        NewBox(1),
        Let(
          "i",
          NewBox(3),
          Let(
            "res",
            NewBox(1),
            Seqn(
              Until0(
                OpenBox("f"),
                Seqn(
                  SetBox("res", Add(OpenBox("res"), OpenBox("i"))),
                  Seqn(
                    SetBox("i", Add(OpenBox("i"), -1)),
                    If0(OpenBox("i"), SetBox("f", 0), 0)
                  )
                )
              ),
              OpenBox("res")
            )
          )
        )
      )
    )
    assertEquals(Num(7), result)
  }
}
