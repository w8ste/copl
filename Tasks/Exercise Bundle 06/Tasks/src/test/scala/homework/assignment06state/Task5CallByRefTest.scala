package homework.assignment06state

import homework.assignment06state.Task5CallByRef.*
import munit.FunSuite

class Task5CallByRefTest extends FunSuite {

  import scala.language.implicitConversions

  implicit def idToSCFLAE(id: String): SCFLAE = Id(id)
  implicit def numToSCFLAE(n: Int): SCFLAE    = Num(n)

  val swap: Fun =
    Fun(
      "x",
      Fun(
        "y",
        Let(
          "t",
          "x",
          Seqn(
            SetId("x", "y"),
            SetId("y", "t")
          )
        )
      )
    )

  test("simple set") {
    val (result, _) = interp(
      Let(
        "x",
        5,
        Seqn(
          SetId("x", 42),
          "x"
        )
      )
    )
    assertEquals(result, Num(42))
  }

  test("nested set") {
    val (result, _) = interp(
      Let(
        "x",
        5,
        Seqn(
          SetId("x", Seqn(SetId("x", 23), Add("x", "x"))),
          "x"
        )
      )
    )
    assertEquals(result, Num(46))
  }

  test("non-ref swap") {
    val (result, _) = interp(
      Let(
        "swap",
        swap,
        Let(
          "x",
          23,
          Let(
            "y",
            42,
            Seqn(
              App(App("swap", "x"), "y"),
              "x"
            )
          )
        )
      )
    )
    assertEquals(result, Num(23))
  }

  test("boxes as variable values") {
    val (result, _) = interp(
      Let(
        "x",
        NewBox(3),
        Let(
          "y",
          NewBox(5),
          Seqn(
            SetId("y", "x"),
            Seqn(
              SetBox("y", 10),
              OpenBox("x")
            )
          )
        )
      )
    )
    assertEquals(result, Num(10))
  }

  test("functions as variable values") {
    val (result, _) = interp(
      Let(
        "x",
        Fun("x", Add("x", 1)),
        Let(
          "y",
          Fun("y", Add("x", 5)),
          Seqn(
            SetId("y", "x"),
            App("y", 3)
          )
        )
      )
    )
    assertEquals(result, Num(4))
  }

  test("side effects on right side of assignment") {
    val (result, _) = interp(
      Let(
        "x",
        0,
        Let(
          "y",
          0,
          Seqn(
            SetId("x", SetId("y", 5)),
            Add("x", "y")
          )
        )
      )
    )
    assertEquals(result, Num(10))
  }

  test("error on Ref in non-arg position") {
    try interp(Let("x", 3, Ref("x")))
    catch {
      case e: MatchError       => fail("match error")
      case e: RuntimeException =>
    }
  }

  test("ref swap") {
    val (result, _) = interp(
      Let(
        "swap",
        swap,
        Let(
          "x",
          23,
          Let(
            "y",
            42,
            Seqn(
              App(App("swap", Ref("x")), Ref("y")),
              "x"
            )
          )
        )
      )
    )
    assertEquals(result, Num(42))
  }

  test("using ref as r-value") {
    val (result, _) = interp(
      Let("y", 5, App(Fun("x", "x"), Ref("y")))
    )
    assertEquals(result, Num(5))
  }

  test("function as reference argument") {
    val (result, _) = interp(
      Let("y", Fun("x", Add("x", "x")), App(App(Fun("x", "x"), Ref("y")), Num(1)))
    )
    assertEquals(result, Num(2))
  }

  test("evaluation of function part of application extends store") {
    val (result, _) = interp(
      Let("z", 4, App(Let("y", 3, Fun("x", Add("x", "y"))), Ref("z")))
    )
    assertEquals(result, Num(7))
  }

  test("env from callsite does not spill into function evaluation") {
    intercept[Exception] {
      interp(
        Let("f", Fun("x", Add("x", "a")), Let("a", 1, Let("b", 2, App("f", Ref("b")))))
      )
    }
  }

  test("basic function evaluation by ref") {
    val (result, _) = interp(
      Let("y", 5, Seqn(App(Fun("x", SetId("x", 6)), Ref("y")), Id("y")))
    )
    assertEquals(result, Num(6))
  }

  test("basic function evaluation by value") {
    val (result, _) = interp(
      Let("y", 5, Seqn(App(Fun("x", SetId("x", 6)), "y"), Id("y")))
    )
    assertEquals(result, Num(5))
  }

}
