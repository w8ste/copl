package interpreters.state.refcount

import scala.language.implicitConversions

class GarbageCollectedWithRefCountTest extends munit.FunSuite {

  import interpreters.memory.refcount.ReferenceCounted.*

  implicit def idToSCFLAE(id: String): Id = Id(id)
  implicit def numToSCFLAE(n: Int): Num   = Num(n)

  test("Num") {
    val p            = Num(1)
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    assertEquals(store.size, 1)
  }

  test("Add") {
    val p            = Add(Num(1), Num(2))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(3))
    // Assert that the operands have been freed
    assertEquals(store.size, 1)
  }

  test("Mult") {
    val p            = Mult(Num(1), Num(2))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    // Assert that the operands have been freed
    assertEquals(store.size, 1)
  }

  test("Let and Id") {
    val p            = Let("x", Num(1), "x")
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    assertEquals(store.size, 1)
  }

  test("Let nested in namedExpr") {
    val p            = Let("x", Let("y", Num(2), Num(1)), "x")
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    assertEquals(store.size, 1)
  }

  test("Let nested in body") {
    val p            = Let("x", Num(1), Let("y", Num(2), "x"))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    assertEquals(store.size, 1)
  }

  test("Let copy semantics") {
    // Second let causes a copy of the value (x and y point to different locations), therefore the SetId only
    // affects one of the bindings.
    val p            = Let("x", 1, Let("y", "x", Seqn(SetId("x", 2), "y")))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    assertEquals(store.size, 1)
  }

  test("Box") {
    val p            = NewBox(Num(1))
    val (loc, store) = interp(p)
    assert(store.lookup(loc).isInstanceOf[Box])
    assertEquals(store.size, 2)
  }

  test("Box nested") {
    val p            = NewBox(NewBox(Num(1)))
    val (loc, store) = interp(p)
    assert(store.lookup(loc).isInstanceOf[Box])
    assertEquals(store.size, 3)
  }

  test("OpenBox") {
    val p            = OpenBox(NewBox(Num(1)))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    // Boxes should be cleaned up
    assertEquals(store.size, 1)
  }

  test("OpenBox 2") {
    val p            = OpenBox(OpenBox(NewBox(NewBox(Num(1)))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    // Boxes should be cleaned up
    assertEquals(store.size, 1)
  }

  test("OpenBox 3") {
    val p            = OpenBox(OpenBox(OpenBox(NewBox(NewBox(NewBox(Num(1)))))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    // Boxes should be cleaned up
    assertEquals(store.size, 1)
  }

  test("OpenBox 4") {
    val p            = OpenBox(NewBox(OpenBox(NewBox(Num(1)))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    // Boxes should be cleaned up
    assertEquals(store.size, 1)
  }

  test("OpenBox value semantics") {
    // OpenBox creates a copy of the value contained in the box, therefore SetBox does not affect the value
    // pointed to by y.
    val p =
      Let("x", NewBox(Num(1)), Let("y", OpenBox("x"), Seqn(SetBox("x", 2), "y")))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    assertEquals(store.size, 1)
  }

  test("OpenBox value semantics 2: nested box") {
    val p =
      Let("x", NewBox(NewBox(1)), Let("y", OpenBox("x"), "y"))
    val (loc, store) = interp(p)
    assert(store.lookup(loc).isInstanceOf[Box])
    assertEquals(store.lookup(store.lookup(loc).asInstanceOf[Box].location), Num(1))
    assertEquals(store.size, 2)
  }

  test("OpenBox value semantics 2: nested closure") {
    // OpenBox creates a copy of the closure which means that a is then referenced by two closures (refCount = 2).
    val p =
      Let("a", 1, Let("x", NewBox(Fun("_", "a")), Let("y", OpenBox("x"), "y")))
    val (loc, store) = interp(p)
    assert(store.lookup(loc).isInstanceOf[Closure])
    assertEquals(store.lookup(loc).asInstanceOf[Closure].env.values.map(store.lookup(_)), List(Num(1)))
    assertEquals(store.size, 2)
  }

  test("Box reference semantics") {
    // This copies the box. x and y point to the same location, therefore SetBox affects both.
    val p =
      Let("x", NewBox(Num(1)), Let("y", "x", Seqn(SetBox("x", 2), OpenBox("y"))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1)
  }

  test("SetBox with different type (simplified), prevent memory leak") {
    // x --> Box --> Box --> Num(1)
    // By setting the content of x to a Num when it contained a box before, the Num(1) might leak.
    val p            = SetBox(NewBox(NewBox(Num(1))), 2)
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1, store)
  }

  test("SetBox with different type, prevent memory leak") {
    // x --> Box --> Box --> Num(1)
    // By setting the content of x to a Num when it contained a box before, the Num(1) might leak.
    val p =
      Let("x", NewBox(NewBox(Num(1))), Seqn(SetBox("x", 2), OpenBox("x")))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1, store)
  }

  test("SetBox with different type, prevent memory leak (closure version)") {
    // x --> Box --> Closure
    // By setting the content of x to a Num when it contained a closure before, the Num(1) might leak.
    val p =
      Let("a", Num(1), Let("x", NewBox(Fun("x", "x")), Seqn(SetBox("x", 2), OpenBox("x"))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1)
  }

  test("SetBox with different type, prevent memory leak (Rec closure version) simplified") {
    // x --> Box --> Closure
    // By setting the content of x to a Num when it contained a closure before, the Num(1) might leak.
    val p            = Let("x", NewBox(LetRec("f", Fun("x", "x"), "f")), SetBox("x", 2))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1)
  }

  test("SetBox with different type, prevent memory leak (Rec closure version)") {
    // x --> Box --> Closure
    // By setting the content of x to a Num when it contained a closure before, the Num(1) might leak.
    val p =
      Let("a", Num(1), Let("x", NewBox(LetRec("f", Fun("x", "x"), "f")), Seqn(SetBox("x", 2), OpenBox("x"))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1)
  }

  test("SetBox with different type, no memory leak") {
    // Like above, but the inner Num has another reference pointing to it so it shouldn"t" be removed.
    val p =
      Let(
        "a",
        Num(1),
        Let(
          "x",
          NewBox(NewBox("a")),
          Seqn(
            SetBox("x", Num(2)), // Num(1) is possibly removed here if implementation is incorrect
            Seqn(
              Add(Num(10), Num(11)), // Litter some values onto the store, would overwrite Num(1) if it had been freed.
              Add("a", OpenBox("x"))
            )
          )
        )
      )
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(3))
    assertEquals(store.size, 1)
  }

  test("Closure") {
    val p =
      Let("x", 1, Let("y", 2, Let("f", Fun("z", Add("x", Add("y", "z"))), "f")))
    val (loc, store) = interp(p)
    assert(store.lookup(loc).isInstanceOf[Closure])
    assertEquals(store.lookup(loc).asInstanceOf[Closure].env.values.map(store.lookup(_)), List(Num(1), Num(2)))
    // The values 1 and 2 are referenced by the environment of the closure,
    // which is why they should not be removed from the store.
    assertEquals(store.size, 3)
  }

  test("Closure shadowing") {
    // For simplicity, the binding y is captured by the closure even though it is technically not possible
    // to access it since it is shadowed by the parameter of the function.
    val p =
      Let("x", 1, Let("y", 2, Let("f", Fun("y", Add("x", "y")), "f")))
    val (loc, store) = interp(p)
    assert(store.lookup(loc).isInstanceOf[Closure])
    assertEquals(store.lookup(loc).asInstanceOf[Closure].env.values.map(store.lookup(_)), List(Num(1), Num(2)))
    assertEquals(store.size, 3)
  }

  test("Closure shadowing + application") {
    val p =
      Let("x", 1, Let("y", 2, Let("f", Fun("y", Add("x", "y")), App("f", 3))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(4))
    assertEquals(store.size, 1)
  }

  test("Closure application") {
    val p =
      Let("x", 1, Let("y", 2, Let("f", Fun("z", Add("x", Add("y", "z"))), App("f", 3))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(6))
    assertEquals(store.size, 1)
  }

  test("Closure call-by-value semantics") {
    // This function application should not affect x.
    val p =
      Let("x", 1, Let("f", Fun("a", SetId("a", 2)), Seqn(App("f", "x"), "x")))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    assertEquals(store.size, 1)
  }

  test("Closure call-by-value semantics (with box)") {
    // Since the box is copied but not its contents, the function application affects the box contents.
    val p =
      Let("x", NewBox(1), Let("f", Fun("a", SetBox("a", 2)), Seqn(App("f", "x"), OpenBox("x"))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1)
  }

  test("Seqn") {
    // Num(1) should be freed. Seqn evaluates to the second expression.
    val p            = Seqn(Num(1), Num(2))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1)
  }

  test("SetId") {
    // Here, the result is the value returned by SetId.
    val p            = Let("x", Num(1), SetId("x", Num(2)))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1)
  }

  test("SetId 2") {
    // Here, the result is the new value of x.
    val p            = Let("x", Num(1), Seqn(SetId("x", Num(2)), "x"))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1)
  }

  test("SetId with different type, prevent memory leak") {
    // x --> Box --> Num(1)
    // By setting x to a Num when it was a box before, the Num(1) might leak.
    val p =
      Let("x", NewBox(Num(1)), Seqn(SetId("x", 2), "x"))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1)
  }

  test("SetId with different type, prevent memory leak (closure version)") {
    // x --> Closure
    // By setting x to a Num when it was a closure before, the Num(1) might leak.
    val p =
      Let("a", Num(1), Let("x", Fun("_", "_"), Seqn(SetId("x", 2), "x")))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1)
  }

  test("SetId with different type, no memory leak") {
    // Like above, but the inner Num has another reference pointing to it so it shouldn"t" be removed.
    val p =
      Let(
        "a",
        Num(1),
        Let(
          "x",
          NewBox("a"),
          Seqn(
            SetId("x", Num(2)), // Num(1) possibly removed by this if implementation is incorrect
            Seqn(
              Add(Num(10), Num(11)), // Litter some values onto the store, would overwrite Num(1) if it was freed.
              Add("a", "x")
            )
          )
        )
      )
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(3))
    assertEquals(store.size, 1)
  }

  test("If0") {
    // The test value should be freed.
    val p            = If0(Num(0), Num(1), Num(2))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    assertEquals(store.size, 1)
  }

  test("Rec: copy semantics") {
    // Make sure Rec behaves like Let:
    // Second let causes a copy of the value (x and y point to different locations), therefore the SetId only
    // affects one of the bindings.
    val p            = LetRec("x", 1, LetRec("y", "x", Seqn(SetId("x", 2), "y")))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    assertEquals(store.size, 1)
  }

  test("Rec: Box reference semantics") {
    // Make sure Ref behaves like Let:
    // This copies the box. x and y point to the same location, therefore SetBox affects both.
    val p =
      LetRec("x", NewBox(Num(1)), LetRec("y", "x", Seqn(SetBox("x", 2), OpenBox("y"))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 1)
  }

  //////////////////////////////////////////////////////
  // LEAKY TESTS BELOW
  //////////////////////////////////////////////////////

  // these are not, and cannot be correctly handed by reference counting

  test("SetId with different type, prevent memory leak (Rec closure version)") {
    // x --> Closure
    // By setting x to a Num when it was a closure before, the Num(1) might leak.
    val p =
      Let("a", Num(1), Let("x", LetRec("f", Fun("x", "x"), "f"), Seqn(SetId("x", 2), "x")))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(2))
    assertEquals(store.size, 3, store)
  }

  test("Rec factorial") {
    val p            = LetRec("fac", Fun("i", If0("i", 1, Mult("i", App("fac", Add("i", -1))))), App("fac", 5))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(5 * 4 * 3 * 2 * 1))
    assertEquals(store.size, 2, store)
  }

  test("Rec factorial by proxy") {
    val p =
      LetRec(
        "fac",
        Fun("i", If0("i", 1, Mult("i", App("fac", Add("i", -1))))),
        LetRec("facProxy", "fac", App("facProxy", 5))
      )
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(5 * 4 * 3 * 2 * 1))
    assertEquals(store.size, 2, store)
  }

  test("Copying recursive closure") {
    val p =
      Let("a", Num(1), Let("b", Num(2), LetRec("f2", LetRec("f", Fun("_", "_"), "f"), App("f2", 3))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(3))
    assertEquals(store.size, 5, store)
  }

  test("Copying recursive closure (factorial)") {
    val fac = LetRec("fac", Fun("i", If0("i", 1, Mult("i", App("fac", Add("i", -1))))), "fac")
    val p   =
      Let("a", Num(1), Let("b", Num(2), LetRec("f2", fac, App("f2", 3))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(3 * 2 * 1))
    assertEquals(store.size, 5, store)
  }

  test("Using recursive closure as function argument") {
    val fac = LetRec("fac", Fun("i", If0("i", 1, Mult("i", App("fac", Add("i", -1))))), "fac")
    val p   =
      Let("f", fac, Let("f2", Fun("function", App("function", 3)), App("f2", "f")))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(3 * 2 * 1))
    assertEquals(store.size, 2, store)
  }

  test("Copying recursive closure (factorial) with Let") {
    val fac = LetRec("fac", Fun("i", If0("i", 1, Mult("i", App("fac", Add("i", -1))))), "fac")
    val p   =
      Let("a", Num(1), Let("b", Num(2), Let("f2", fac, App("f2", 3))))
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(3 * 2 * 1))
    assertEquals(store.size, 4, store)
  }

  test("Copying recursive closure (deep) simplified") {
    val p =
      LetRec(
        "f3",
        LetRec(
          "f2",
          LetRec(
            "f",
            Fun("x", "x"),
            "f"
          ),
          "f2"
        ),
        App("f3", 3)
      )
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(3))
    assertEquals(store.size, 4, store)
  }

  test("Copying recursive closure (deep)") {
    val p =
      Let(
        "a",
        Num(1),
        Let(
          "b",
          Num(2),
          LetRec(
            "f3",
            LetRec(
              "f2",
              LetRec("f", Fun("x", "x"), "f"),
              "f2"
            ),
            App("f3", 3)
          )
        )
      )
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(3))
    assertEquals(store.size, 6, store)
  }

  test("Box cycle") {
    val p = Seqn(
      Let("x", NewBox(10), Let("y", NewBox("x"), SetBox("x", "y"))),
      Num(1)
    )
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    assertEquals(store.size, 3, store)
  }

  test("Box cycle (deep)") {
    val p = Seqn(
      Let("x", NewBox(10), Let("y", NewBox("x"), Let("z", NewBox("y"), SetBox("x", "z")))),
      Num(1)
    )
    val (loc, store) = interp(p)
    assertEquals(store.lookup(loc), Num(1))
    assertEquals(store.size, 4, store)
  }
}
