package interpreters.state

class StateTests extends munit.FunSuite {

  import State.*
  import Standard.*

  import scala.language.implicitConversions

  implicit def idToSCFLAE(id: String): Id = Id(id)
  implicit def numToSCFLAE(n: Int): Num   = Num(n)

  test("lecture example") {
    val exp          = Num(42)
    val (res, store) = interp(App(Fun("b1", App(Fun("b2", exp), NewBox(5))), NewBox(5)))
    assertEquals(res, exp)
    assertEquals(store, Map(1 -> Num(5), 2 -> Box(1), 3 -> Num(5), 4 -> Box(3)))
  }

  test("all") {

    val (tv1, _) = interp(Let("a", NewBox(1), OpenBox("a")))
    assertEquals(tv1, Num(1))

    val (tv2, _) = interp(
      Let("a", NewBox(1), Let("f", Fun("x", Add("x", OpenBox("a"))), Seqn(SetBox("a", 2), App("f", 5))))
    )
    assertEquals(tv2, Num(7))

    val (tv3, _) = interp(
      Let(
        "switch",
        NewBox(0),
        Let(
          "toggle",
          Fun("dummy", If0(OpenBox("switch"), Seqn(SetBox("switch", 1), 1), Seqn(SetBox("switch", 0), 0))),
          Add(App("toggle", 42), App("toggle", 42))
        )
      )
    )
    assertEquals(tv3, Num(1))

    val (tv4, _) = interp(
      Let(
        "switch",
        0,
        Let(
          "toggle",
          Fun("dummy", If0("switch", Seqn(SetId("switch", 1), 1), Seqn(SetId("switch", 0), 0))),
          Add(App("toggle", 42), App("toggle", 42))
        )
      )
    )
    assertEquals(tv4, Num(1))

    val (tv5, ts5) = interp(
      App(Fun("b1", App(Fun("b2", Seqn(SetBox("b1", 6), OpenBox("b2"))), NewBox(7))), NewBox(5))
    )
    assertEquals(tv5, Num(7))
    assertEquals(ts5(1), Num(6))

    val (tv6, _) = interp(
      Let("b", 0, If0(Seqn(SetId("b", 5), "b"), 1, "b"))
    )
    assertEquals(tv6, Num(5))

    val (tv7, _) = interp(Let("b", 4, Add("b", Seqn(SetId("b", 5), "b"))))
    assertEquals(tv7, Num(9))

  }
}
