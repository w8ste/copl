package modularized

class ImmutableEnvironmentTests extends munit.FunSuite {

  import modularized.environmental.FullInterpreter.*

  test("all") {

    assertEquals(
      interp(
        LetRec("fact", TypedFun("n", TNum(), If0("n", 1, Mult("n", App("fact", Sub("n", 1))))), App("fact", 5))
      ),
      Num(120)
    )
    {
      val res = interp(
        Let("x", 3, TypedFun("y", TNum(), Add("x", "y")))
      )
      assert {
        res match
            case Closure("y", Add(Id("x"), Id("y")), env) =>
              assertEquals(env("x"), Num(3))
              true
            case other => false
      }

    }
    assertEquals(
      interp(
        Let("inc", TypedFun("x", TNum(), Add("x", 1)), Add(App("inc", 4), App("inc", 5)))
      ),
      Num(11)
    )
    assertEquals(
      interp(
        Let("inc", TypedFun("x", TNum(), Add("x", 1)), "inc")
      ),
      Closure("x", Add("x", 1), emptyEnvironment)
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
}
