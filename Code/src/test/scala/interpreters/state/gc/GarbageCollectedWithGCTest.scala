package interpreters.state.gc

import interpreters.memory.RootedEnv
import interpreters.memory.stores.{MarkAndSweepStore, Store}
import modularized.stateful.GarbageCollected
import modularized.stateful.GarbageCollected.*

class GarbageCollectedWithGCTest extends munit.FunSuite {

  import scala.language.implicitConversions
  implicit def idToSRCFLAE(id: String): Id                       = Id(id)
  implicit def numToSRCFLAE(n: Int): Num                         = Num(n)
  implicit def rootedMap(dict: Map[String, Location]): RootedEnv = RootedEnv(dict, dict.values.toSet)

  val emptyStack: RootedEnv = RootedEnv.empty

  def interp(value: Expr, alwaysGc: Boolean = true): (Value, Store[Value]) =
    GarbageCollected.interp(value, RootedEnv.empty).run(MarkAndSweepStore(100, locationsInValue, alwaysGc))

  // New store tests:

  test("MarkAndSweepStore: Malloc") {
    val s1      = MarkAndSweepStore(2, locationsInValue)
    val (_, s2) = s1.malloc(Num(42), Set.empty)
    assert(s2.remainingCapacity == 1)
    assert(s2.memory.contains(Some(Num(42))))
  }

  test("MarkAndSweepStore: Malloc causes GC") {
    val s1      = MarkAndSweepStore(2, locationsInValue, alwaysGc = false)
    val (_, s2) = s1.malloc(Num(1), Set.empty)
    val (_, s3) = s2.malloc(Num(2), Set.empty)
    assert(s3.remainingCapacity == 0)
    // since there is no space left, GC is triggered which sweeps all values (since the stack is empty)
    val (_, s4) = s3.malloc(Num(3), Set.empty)
    assert(s4.remainingCapacity == 1)
    assert(s4.memory.flatten.contains(Num(3)))
    assert(!s4.memory.flatten.contains(Num(1)))
    assert(!s4.memory.flatten.contains(Num(2)))
  }

  test("MarkAndSweepStore: Malloc causes GC 2") {
    val s1      = MarkAndSweepStore(2, locationsInValue, alwaysGc = false)
    val (_, s2) = s1.malloc(Num(1), Set.empty)
    val (_, s3) = s2.malloc(Num(2), Set.empty)
    assert(s3.remainingCapacity == 0)
    // here, only one value is swept, "2" is kept since there is a variable pointing to it
    val (_, s4) = s3.malloc(Num(3), Set(1))
    assert(s4.remainingCapacity == 0)
    assert(s4.memory.flatten.contains(Num(3)))
    assert(!s4.memory.flatten.contains(Num(1)))
    assert(s4.memory.flatten.contains(Num(2)))
  }

  test("MarkAndSweepStore: Out of memory after malloc") {
    val s1      = MarkAndSweepStore(2, locationsInValue, alwaysGc = false)
    val (_, s2) = s1.malloc(Num(1), Set.empty)
    val (_, s3) = s2.malloc(Num(2), Set.empty)
    assert(s3.remainingCapacity == 0)
    // no value can be collected, which means there is no memory for "3"
    intercept[Exception] {
      s3.malloc(Num(3), Set(0, 1))
    }
  }

  test("MarkAndSweepStore: Deep sweep") {
    val s1      = MarkAndSweepStore(4, locationsInValue)
    val (_, s2) = s1.malloc(Num(1), Set.empty)
    val (_, s3) = s2.malloc(Box(0), Set.empty)
    val (_, s4) = s3.malloc(Box(1), Set.empty)
    val (_, s5) = s4.malloc(Box(2), Set.empty)
    // GC happens here
    val (_, s6) = s5.malloc(Num(2), Set.empty)
    assert(s6.remainingCapacity == 3)
  }

  test("MarkAndSweepStore: Deep sweep with closure") {
    val s1      = MarkAndSweepStore(4, locationsInValue)
    val (_, s2) = s1.malloc(Num(1), Set.empty)
    val (_, s3) = s2.malloc(Box(0), Set.empty)
    val (_, s4) = s3.malloc(Closure("param", Num(100), Map("x" -> 1)), Set.empty)
    val (_, s5) = s4.malloc(Box(2), Set.empty)
    // GC happens here
    val (_, s6) = s5.malloc(Num(2), Set.empty)
    assert(s6.remainingCapacity == 3)
  }

  test("MarkAndSweepStore: Loop 0") {
    val s1 = MarkAndSweepStore[Value](1, Vector(Some(Box(0))), locationsInValue)

    intercept[Exception] {
      s1.malloc(Num(1), Set(0))
    }
  }

  test("MarkAndSweepStore: Loop 1") {
    val s1 = MarkAndSweepStore[Value](2, Vector(Some(Box(1)), Some(Box(0))), locationsInValue)

    intercept[Exception] {
      s1.malloc(Num(1), Set(0))
    }
  }

  test("MarkAndSweepStore: Loop 2") {
    val s1 = MarkAndSweepStore[Value](3, Vector(Some(Box(1)), Some(Box(2)), Some(Box(0))), locationsInValue)

    intercept[Exception] {
      s1.malloc(Num(1), Set(0))
    }
  }

  // New interpreter tests:

  test("Interpreter: Box GC") {
    val store   = MarkAndSweepStore(2, locationsInValue)
    val program = Seqn(Let("a", NewBox(42), "a"), Let("b", NewBox(43), "b"))
    val (v, s)  = GarbageCollected.interp(program, RootedEnv.empty).run(store)
    // println("Store = " + s)
    val Box(location) = v: @unchecked
    assertEquals(s.remainingCapacity, 0)
    val gcStore = s.gc(Set(location))
    assertEquals(gcStore.lookup(location), Num(43))
    assertEquals(gcStore.remainingCapacity, 1)
  }

  // Old interpreter tests:

  test("many boxes") {
    val expr = Let(
      "res",
      Let(
        "x1",
        NewBox(10),
        Let(
          "x2",
          NewBox("x1"),
          Let(
            "x3",
            NewBox(11),
            Let(
              "x4",
              NewBox("x3"),
              Let("x5", NewBox("x1"), Let("x6", NewBox("x4"), Let("x7", NewBox("x6"), NewBox("x6"))))
            )
          )
        )
      ),
      "res"
    )

    val (loc, store) = interp(expr, alwaysGc = false)
    assertEquals(store.remainingCapacity, 84)
    assertEquals(store.gc(Set(15)).remainingCapacity, 95)
  }

  test("Interpreter: NewBox, OpenBox") {
    val (tv1, ts1) = interp(Let("a", NewBox(1), OpenBox("a")))
    assert(tv1 == Num(1))
  }

  test("Interpreter: Closure, SetBox") {
    val (tv2, ts2) = interp(
      Let("a", NewBox(1), Let("f", TypedFun("x", TNum(), Add("x", OpenBox("a"))), Seqn(SetBox("a", 2), App("f", 5))))
    )
    assert(tv2 == Num(7))
  }

  test("Interpreter: Closure, SetBox 2") {
    val (tv3, ts3) = interp(
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
  }

  test("Interpreter: Closure, SetId") {
    val (tv4, ts4) = interp(
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
  }

  test("Interpreter: Nested function application with boxes") {
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
    assertEquals(tv5, Num(7))
    assertEquals(ts5.lookup(0), Num(6))
  }

  test("Interpreter: SetId in If0 condition") {
    val (tv6, ts6) = interp(
      Let("b", 0, If0(Seqn(SetId("b", 5), "b"), 1, "b"))
    )
    assert(tv6 == Num(5))
  }

  test("Interpreter: SetId in addition") {
    val (tv7, ts7) = interp(Let("b", 4, Add("b", Seqn(SetId("b", 5), "b"))))
    assert(tv7 == Num(9))
  }

  test("Interpreter: Factorial") {
    val (tv, ts) = interp(
      LetRec("fact", TypedFun("n", TNum(), If0("n", 1, Mult("n", App("fact", Add("n", -1))))), App("fact", 5))
    )
    assert(tv == Num(120))
  }

  test("shadowed bindings are not collected") {
    val prog =
      Let(
        "x",
        Num(120),
        Seqn(
          Let("x", Num(121), Let("something", Num(42), Num(1))),
          Id("x")
        )
      )
    intercept[Exception] {
      val (resV, _) = GarbageCollected.interp(prog, RootedEnv.empty).run(MarkAndSweepStore(2, locationsInValue))
      assertEquals(resV, Num(120))
    }

  }

}
