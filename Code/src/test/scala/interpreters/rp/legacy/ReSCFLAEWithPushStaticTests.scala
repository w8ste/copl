package interpreters.rp.legacy

class ReSCFLAEWithPushStaticTests extends munit.FunSuite {

  import interpreters.rp.legacy.ReSCFLAEWithPushStatic.*

  import scala.language.implicitConversions
  implicit def idToSCFLAE(id: String): Id = Id(id)
  implicit def numToSCFLAE(n: Int): Num   = Num(n)
  // Standard tests of the reactive interpreter

  test("test1") {
    assertEquals(Num(2), interp(Let("f", Fun("x", Add("x", "x")), App("f", If0(0, 1, 2))))._1)
  }

  test("test2") {
    assertEquals(Num(1), interp(CurrVal(NewVar(1)))._1)
  }

  test("test3") {
    assertEquals(
      true, {
        interp(Let("x", NewVar(NewVar(1)), CurrVal("x")))._1 match {
          case Var(_, _, _) => true
          case _            => false
        }
      }
    )
  }

  test("test4") {
    assertEquals(Num(2), interp(CurrVal(NewVar(Let("x", If0(0, 1, 2), Add("x", "x")))))._1)
  }

  test("test5") {
    assertEquals(Num(2), interp(Let("x", 1, CurrVal(Let("x", 2, NewVar("x")))))._1)
  }

  test("test6") {
    assertEquals(Num(2), interp(CurrVal(Let("x", NewVar(1), Seqn(SetVar("x", 2), "x"))))._1)
  }

  test("test7") {
    assertEquals(Num(3), interp(Let("x", NewVar(1), Let("y", Add("x", 1), Seqn(SetVar("x", 2), CurrVal("y")))))._1)
  }

  test("test8") {
    assertEquals(
      Num(4), {
        interp(Let(
          "x",
          NewVar(1),
          Let(
            "y",
            NewVar(2),
            Let(
              "z",
              NewVar(3),
              Let("if", If0("x", "y", "z"), Seqn(Seqn(SetVar("x", 0), SetVar("y", 4)), CurrVal("if")))
            )
          )
        ))._1
      }
    )
  }

  test("test9") {
    assertEquals(
      Num(5), {
        interp(Let(
          "x",
          NewVar(1),
          Let("y", NewVar(2), Let("z", Add("x", "y"), Add(CurrVal("z"), Seqn(SetVar("y", CurrVal("x")), CurrVal("z")))))
        ))._1
      }
    )
  }

  test("test10") {
    assertEquals(
      Num(2), {
        interp(Let(
          "v",
          NewVar(0),
          Let(
            "f",
            Fun("x", If0("x", "x", Add("x", 1))),
            Let("res", App("f", "v"), Seqn(SetVar("v", 1), CurrVal("res")))
          )
        ))._1
      }
    )
  }

  test("test11") {
    assertEquals(
      Num(1), {
        interp(Let(
          "v",
          NewVar(0),
          Let(
            "f",
            If0("v", Fun("x", Add("x", -1)), Fun("x", Add("x", 1))),
            Let("res", App("f", 0), Seqn(SetVar("v", 1), CurrVal("res")))
          )
        ))._1
      }
    )
  }

  test("testTime") {
    interp(Let("t0", CurrVal(Time()), Let("tnow", Time(), Seqn(Wait(100), CurrVal(Sub("tnow", "t0"))))))._1 match {
      case Num(n) => assert(n >= 100, s"$n")
      case other  => assert(false)
    }
  }
}
