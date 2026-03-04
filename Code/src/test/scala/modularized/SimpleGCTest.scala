package modularized

import interpreters.memory.RootedEnv
import interpreters.memory.stores.{MarkAndSweepStore, Store}
import modularized.stateful.GarbageCollected

class SimpleGCTest extends munit.FunSuite {

  import modularized.stateful.GarbageCollected.*

  def interpStore(expr: Expr): (Value, Store[Value]) =
    GarbageCollected.interp(expr, RootedEnv.empty).run(MarkAndSweepStore(1000, locationsInValue))

  def interp(expr: Expr): Value =
      val (value, store) = interpStore(expr)
      value

  def interp2(
      expr: Expr,
      stack: RootedEnv = RootedEnv.empty
  ): Value = {
    val (v, s) = GarbageCollected.interp(expr, stack).run(MarkAndSweepStore(100, locationsInValue))
    v
  }

  test("all") {

    assertEquals(
      interp(
        LetRec("fact", TypedFun("n", TNum(), If0("n", 1, Mult("n", App("fact", Sub("n", 1))))), App("fact", 5))
      ),
      Num(120)
    )
    {
      val res = interpStore(
        Let("x", 3, TypedFun("y", TNum(), Add("x", "y")))
      )
      assert {
        res match
            case (Closure("y", Add(Id("x"), Id("y")), env), store) =>
              assertEquals(store.lookup(env("x")), Num(3))
              true
            case other => false
      }

    }
    assertEquals(
      interp(
        Let("inc", TypedFun("x", TNum(), Add("x", 1)), App("inc", 4))
      ),
      Num(5)
    )
    assertEquals(
      interp(
        Let("inc", TypedFun("x", TNum(), Add("x", 1)), "inc")
      ),
      Closure("x", Add("x", 1), RootedEnv.empty)
    )
    assertEquals(
      interp(
        Let("inc", TypedFun("x", TNum(), Add("x", 1)), Add(App("inc", 4), App("inc", 5)))
      ),
      Num(11)
    )
    assertEquals(interp(Let("x", 3, App(TypedFun("y", TNum(), Add("x", "y")), 4))), Num(7))

    assertEquals(
      interp(
        LetRec(
          "fact",
          App(TypedFun("x", TNum(), "x"), TypedFun("n", TNum(), If0("n", 1, Mult("n", App("fact", Sub("n", 1)))))),
          App("fact", 5)
        )
      ),
      Num(120)
    )

  }

  test("test1") {
    assertEquals(interp2(Let("a", NewBox(1), OpenBox("a"))), Num(1))
  }

  test("test2") {
    assertEquals(
      interp2(
        Let(
          "a",
          NewBox(1),
          Let("f", TypedFun("x", TNum(), Add("x", OpenBox("a"))), Seqn(SetBox("a", 2), App("f", 5)))
        )
      ),
      Num(7)
    )
  }

  test("test3") {
    assertEquals(
      interp2(
        Let(
          "switch",
          NewBox(0),
          Let(
            "toggle",
            TypedFun(
              "dummy",
              TNum(),
              If0(OpenBox("switch"), Seqn(SetBox("switch", 1), 1), Seqn(SetBox("switch", 0), 0))
            ),
            Add(App("toggle", 42), App("toggle", 42))
          )
        )
      ),
      Num(1)
    )
  }

  test("test4") {
    assertEquals(
      interp2(
        Let(
          "switch",
          0,
          Let(
            "toggle",
            TypedFun("dummy", TNum(), If0("switch", Seqn(SetId("switch", 1), 1), Seqn(SetId("switch", 0), 0))),
            Add(App("toggle", 42), App("toggle", 42))
          )
        )
      ),
      Num(1)
    )
  }

  test("test5") {
    assertEquals(
      interp2(
        App(
          TypedFun(
            "b1",
            TBox(TNum()),
            App(TypedFun("b2", TBox(TNum()), Seqn(SetBox("b1", 6), OpenBox("b2"))), NewBox(7))
          ),
          NewBox(5)
        )
      ),
      Num(7)
    )
  }
}
