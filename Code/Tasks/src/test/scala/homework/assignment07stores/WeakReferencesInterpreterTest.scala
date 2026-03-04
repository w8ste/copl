package homework.assignment07stores

import homework.assignment07stores.WeakReferences.*
import interpreters.memory.RootedEnv
import interpreters.memory.stores.MarkAndSweepStore

import scala.language.implicitConversions

class WeakReferencesInterpreterTest extends munit.FunSuite {
  implicit def idToSCFLAE(id: String): Id = Id(id)
  implicit def numToSCFLAE(n: Int): Num   = Num(n)

  val emptyEnv: Env         = Map()
  val emptyStack: RootedEnv = RootedEnv.empty

  test("correct WeakRef") {
    val (res, st) = interp(Let("ref", NewWeakRef(42), "ref"), emptyStack, WeakMarkAndSweep(1000))
    assertEquals(Num(42), st.lookup(0))
    assertEquals(WeakRef(0), st.lookup(1))
  }

  test("invalidates location for WeakRef on gc") {
    val st              = WeakMarkAndSweep(10)
    val (targetLoc, s1) = st.malloc(Num(42), Set.empty)
    val (refLoc, s2)    = s1.malloc(WeakRef(targetLoc), Set.empty)
    val s3              = s2.gc(Set(refLoc))
    assert(
      !s3.contains(targetLoc),
      s"WeakRef location must be collected, but was ${s3.lookup(targetLoc)}"
    )
    assertEquals(s3.lookup(refLoc), WeakRef(targetLoc))
  }

  test("WeakRef and successful Deref") {
    assertEquals(
      {
        val (res, _) = interp(Let("ref", NewWeakRef(42), TryDeref("ref", 23)), emptyStack, WeakMarkAndSweep(1000))
        res
      },
      Num(42)
    )
  }

  test("WeakRef and unsuccessful Deref") {
    assertEquals(
      {
        val st        = WeakMarkAndSweep(2)
        val (res, s1) = interp(
          Let("ref", NewWeakRef(42), Seqn(Seqn(Let("x", -1, "x"), Let("x", -1, "x")), TryDeref("ref", 23))),
          emptyStack,
          st
        )
        res
      },
      Num(23)
    )
  }

  test("Nested WeakRefs") {
    assertEquals(
      {
        val st          = WeakMarkAndSweep(3)
        val (res, sRes) =
          interp(Let("ref", NewWeakRef(NewWeakRef(42)), Seqn(Let("x", -1, "x"), TryDeref("ref", 23))), emptyStack, st)
        (res, sRes.remainingCapacity)
      },
      (Num(23), 1)
    )
  }

  test("GC WeakRef itself and referenced value") {
    assertEquals(
      {
        val st       = WeakMarkAndSweep(2)
        val (res, _) = interp(
          Seqn(Let("ref", NewWeakRef(42), Num(0)), Let("x", -1, "x")),
          emptyStack,
          st
        )
        res
      },
      Num(-1)
    )
  }

  // This test case checks if multiple runs of GC still work as intended.
  // Notably, this is the only situation where forward references can occur in our language,
  // so the test case verifies this too.
  test("Multiple GCs and forward references") {
    assertEquals(
      {
        val st        = WeakMarkAndSweep(4)
        val (res, s1) = interp(
          Let(
            "ref",
            NewWeakRef(42),
            Seqn(
              Let("ref2", NewWeakRef(23), Let("x", -1, "x")),
              Let("a", 1, Let("b", 2, Let("c", 3, Add("a", "b"))))
            )
          ),
          emptyStack,
          st
        )
        println(
          s"s1:\n\t${s1.asInstanceOf[MarkAndSweepStore[?]].memory}\n\t${s1.asInstanceOf[MarkAndSweepStore[?]].virtualMemory}"
        )
        res
      },
      Num(3)
    )
    intercept[Exception] {
      val st       = WeakMarkAndSweep(4)
      val (res, _) = interp(
        Let(
          "ref",
          NewWeakRef(42),
          Seqn(
            Let("ref2", NewWeakRef(23), Let("x", -1, "x")),
            Let("a", 1, Let("b", 2, Let("c", 3, Let("d", 4, Add("a", "b")))))
          )
        ),
        emptyStack,
        st
      )
      res
    }
  }

  // It may not be obvious but this test case is specifically designed to fail if WeakRefs
  // are still referentially distinct after being set to INVALID_LOC, as if not, GC will "swallow"
  // "ref2" when collecting the content of "ref".
  test("no referential equality of invalidated WeakRefs") {
    assertEquals(
      {
        val st       = WeakMarkAndSweep(4)
        val (res, _) = interp(
          Let(
            "ref",
            NewWeakRef(42),
            Let(
              "ref2",
              NewWeakRef(23),
              Seqn(
                Let("x", -1, "x"),
                TryDeref("ref2", Num(100))
              )
            )
          ),
          emptyStack,
          st
        )
        res
      },
      Num(100)
    )

  }

  test("NewBox and OpenBox") {
    assertEquals(
      {
        val (res, _) = interp(Let("a", NewBox(1), OpenBox("a")), emptyStack, WeakMarkAndSweep(1000))
        res
      },
      Num(1)
    )
  }

  test("Fun and Seq") {
    assertEquals(
      {
        val (res, _) = interp(
          Let("a", NewBox(1), Let("f", Fun("x", Add("x", OpenBox("a"))), Seqn(SetBox("a", 2), App("f", 5)))),
          emptyStack,
          WeakMarkAndSweep(1000)
        )
        res
      },
      Num(7)
    )
  }

  test("If0 & SetBox") {
    assertEquals(
      {
        val (res, _) = interp(
          Let(
            "switch",
            NewBox(0),
            Let(
              "toggle",
              Fun("dummy", If0(OpenBox("switch"), Seqn(SetBox("switch", 1), 1), Seqn(SetBox("switch", 0), 0))),
              Add(App("toggle", 42), App("toggle", 42))
            )
          ),
          emptyStack,
          WeakMarkAndSweep(1000)
        )
        res
      },
      Num(1)
    )
  }

  test("If0 & SetId") {
    assertEquals(
      {
        val (res, _) = interp(
          Let(
            "switch",
            0,
            Let(
              "toggle",
              Fun("dummy", If0("switch", Seqn(SetId("switch", 1), 1), Seqn(SetId("switch", 0), 0))),
              Add(App("toggle", 42), App("toggle", 42))
            )
          ),
          emptyStack,
          WeakMarkAndSweep(1000)
        )
        res
      },
      Num(1)
    )
  }

  test("Multiple Boxes") {
    assertEquals(
      {
        val (res, _) = interp(
          App(Fun("b1", App(Fun("b2", Seqn(SetBox("b1", 6), OpenBox("b2"))), NewBox(7))), NewBox(5)),
          emptyStack,
          WeakMarkAndSweep(1000)
        )
        res
      },
      Num(7)
    )
  }

  test("Correct Seqn return value") {
    assertEquals(
      {
        val (res, _) = interp(
          Let("b", 0, If0(Seqn(SetId("b", 5), "b"), 1, "b")),
          emptyStack,
          WeakMarkAndSweep(1000)
        )
        res
      },
      Num(5)
    )
  }

  test("test7") {
    assertEquals(
      {
        val (res, _) = interp(Let("b", 4, Add("b", Seqn(SetId("b", 5), "b"))), emptyStack, WeakMarkAndSweep(1000))
        res
      },
      Num(9)
    )
  }

}
