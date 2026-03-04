package interpreters.state.gc

import interpreters.memory.RootedEnv
import interpreters.memory.stores.{MinimalStore, Store}
import modularized.stateful.GarbageCollected

class GarbageCollectedTests extends munit.FunSuite {

  import modularized.stateful.GarbageCollected.*

  def interp(value: Expr): (Value, Store[Value]) =
    GarbageCollected.interp(value, RootedEnv.empty).run(new MinimalStore[Value](100))

  test("all") {

    val (tv1, _) = interp(Let("a", NewBox(1), OpenBox("a")))
    assert(tv1 == Num(1))

    val (tv2, _) = interp(
      Let("a", NewBox(1), Let("f", TypedFun("x", TNum(), Add("x", OpenBox("a"))), Seqn(SetBox("a", 2), App("f", 5))))
    )
    assert(tv2 == Num(7))

    val (tv3, _) = interp(
      Let(
        "switch",
        NewBox(0),
        Let(
          "toggle",
          TypedFun("dummy", TNum(), If0(OpenBox("switch"), Seqn(SetBox("switch", 1), 1), Seqn(SetBox("switch", 0), 0))),
          Add(App("toggle", 42), App("toggle", 42))
        )
      )
    )
    assert(tv3 == Num(1))

    val (tv4, _) = interp(
      Let(
        "switch",
        0,
        Let(
          "toggle",
          TypedFun("dummy", TNum(), If0("switch", Seqn(SetId("switch", 1), 1), Seqn(SetId("switch", 0), 0))),
          Add(App("toggle", 42), App("toggle", 42))
        )
      )
    )
    assert(tv4 == Num(1))

    val (tv5, ts5) = interp(
      App(
        TypedFun(
          "b1",
          TBox(TNum()),
          App(TypedFun("b2", TBox(TNum()), Seqn(SetBox("b1", 6), OpenBox("b2"))), NewBox(7))
        ),
        NewBox(5)
      )
    )
    assert(tv5 == Num(7))
    // println(ts5)
    // println(Num(6))
    assert(ts5.lookup(0) == Num(6))

    val (tv6, _) = interp(
      Let("b", 0, If0(Seqn(SetId("b", 5), "b"), 1, "b"))
    )
    assert(tv6 == Num(5))

    val (tv7, _) = interp(Let("b", 4, Add("b", Seqn(SetId("b", 5), "b"))))
    assert(tv7 == Num(9))

    {
      val (v, store) = interp(
        LetRec("fact", TypedFun("n", TNum(), If0("n", 1, Mult("n", App("fact", Add("n", -1))))), App("fact", 5))
      )
      assertEquals(v, Num(120))
    }

    assert(interp(Let("x", 3, TypedFun("y", TNum(), Add("x", "y")))) == (
      Closure(
        "y",
        Add("x", "y"),
        RootedEnv(Map("x" -> 0))
      ),
      MinimalStore(
        100,
        Seq(Num(3))
      )
    ))
    assert(interp(
      Let("inc", TypedFun("x", TNum(), Add("x", 1)), Add(App("inc", 4), App("inc", 5)))
    )._1 == Num(11))
    assert(interp(
      Let("inc", TypedFun("x", TNum(), Add("x", 1)), "inc")
    )._1 == Closure("x", Add("x", 1), RootedEnv.empty))
    assert(interp(Let("x", 3, App(TypedFun("y", TNum(), Add("x", "y")), 4)))._1 == Num(7))
  }

  def whatDoesThisDo(n: Int): Expr = {
    var v: Expr = Num(17)
    Range.inclusive(1, n).foreach(i => v = Seqn(NewBox(i), v))
    v
  }
  test("size OK") {

    val iterations  = 20
    val storeSize   = 20
    val store       = new MinimalStore[Value](storeSize)
    val (_, store2) = GarbageCollected.interp(whatDoesThisDo(iterations), RootedEnv.empty).run(store)
    assertEquals(store2.remainingCapacity, 0)
  }

  test("size not OK") {

    val iterations = 21
    val storeSize  = 20
    val store      = new MinimalStore[Value](storeSize)
    intercept[RuntimeException] {
      GarbageCollected.interp(whatDoesThisDo(iterations), RootedEnv.empty).run(store)
    }
  }

}
