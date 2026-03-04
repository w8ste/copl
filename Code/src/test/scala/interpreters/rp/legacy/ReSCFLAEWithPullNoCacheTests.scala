package interpreters.rp.legacy

class ReSCFLAEWithPullNoCacheTests extends munit.FunSuite {

  import interpreters.rp.legacy.ReSCFLAEWithPullNoCache.*

  test("read nested var") {
    // Can create and read nested var
    assert(interp(Let("x", NewVar(NewVar(1)), CurrVal("x")))._1 match {
      case Var(_) => true
      case _      => false
    })
  }

  test("all") {

    // Standard interpreter test
    assertEquals(interp(Let("f", Fun("x", Add("x", "x")), App("f", If0(0, 1, 2))))._1, Num(2))

    // Can create and read simple var
    assertEquals(interp(CurrVal(NewVar(1)))._1, Num(1))

    // Can create and read complex var
    assertEquals(interp(CurrVal(NewVar(Let("x", If0(0, 1, 2), Add("x", "x")))))._1, Num(2))

    // Static scoping for vars
    assertEquals(interp(Let("x", 1, CurrVal(Let("x", 2, NewVar("x")))))._1, Num(2))

    // Can set new value for var (box-like behavior)
    assertEquals(interp(CurrVal(Let("x", NewVar(1), Seqn(SetVar("x", 2), "x"))))._1, Num(2))

    // Update of dependent values
    assertEquals(interp(Let("x", NewVar(1), Let("y", Add("x", 1), Seqn(SetVar("x", 2), CurrVal("y")))))._1, Num(3))

    // More complex dependency updates
    assertEquals(
      interp(Let(
        "x",
        NewVar(1),
        Let(
          "y",
          NewVar(2),
          Let("z", NewVar(3), Let("if", If0("x", "y", "z"), Seqn(Seqn(SetVar("x", 0), SetVar("y", 4)), CurrVal("if"))))
        )
      ))._1,
      Num(4)
    )
    assertEquals(
      interp(Let(
        "x",
        NewVar(1),
        Let("y", NewVar(2), Let("z", Add("x", "y"), Add(CurrVal("z"), Seqn(SetVar("y", CurrVal("x")), CurrVal("z")))))
      ))._1,
      Num(5)
    )
    assertEquals(
      interp(Let(
        "v",
        NewVar(0),
        Let(
          "f",
          Fun("x", If0("x", "x", Add("x", 1))),
          CurrVal(Let("res", App("f", "v"), Seqn(SetVar("v", 1), CurrVal("res"))))
        )
      ))._1,
      Num(2)
    )
    assertEquals(
      interp(Let(
        "v",
        NewVar(0),
        Let(
          "f",
          If0("v", Fun("x", Add("x", -1)), Fun("x", Add("x", 1))),
          Let("res", App("f", 0), Seqn(SetVar("v", 1), CurrVal("res")))
        )
      ))._1,
      Num(1)
    )
  }
}
