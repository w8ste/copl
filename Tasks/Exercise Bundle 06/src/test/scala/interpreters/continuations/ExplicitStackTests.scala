package interpreters.continuations

class ExplicitStackTests extends munit.FunSuite {

  import ExplicitStackContinuations.*

  import scala.language.implicitConversions
  implicit def symbolToFE(symbol: String): Id = Id(symbol)
  implicit def intToFE(i: Int): Num           = Num(i)

  test("simple") {
    assertEquals(interp(Add(Num(4), Num(6))), Num(10))
  }

  test("fun") {
    assertEquals(interp(App(Fun("n", Add("n", 10)), 14)), Num(24))
  }

  // Z combinator
  val innerZ: Fun = Fun("x", App("f", Fun("y", App(App("x", "x"), "y"))))
  val Z: Fun      = Fun("f", App(innerZ, innerZ))

  def sum(n: KCFLAE): KCFLAE =
    App(
      App(
        Z,
        Fun(
          "rec",
          Fun(
            "n",
            If0("n", 0, Add("n", App("rec", Add("n", -1))))
          )
        )
      ),
      n
    )

  def mul(left: KCFLAE, right: KCFLAE): KCFLAE =
    App(
      App(
        App(
          Z,
          Fun(
            "rec",
            Fun(
              "m",
              Fun(
                "n",
                If0(Add("m", -1), "n", Add("n", App(App("rec", Add("m", -1)), "n")))
              )
            )
          )
        ),
        left
      ),
      right
    )

  def fact(n: KCFLAE): App = App(App(Z, Fun("rec", Fun("n", If0("n", 1, mul("n", App("rec", Add("n", -1))))))), n)

  test("multiplication") {

    assertEquals(interp(mul(1, 2)), Num(2))
    assertEquals(interp(mul(2, 2)), Num(4))
    assertEquals(interp(mul(5, 5)), Num(25))

  }

  test("continuations") {

    assertEquals(interp(BindCC("k", 3)), Num(3))

    assertEquals(interp(BindCC("k", App("k", 3))), Num(3))

    assertEquals(interp(BindCC("k", Add(1, App("k", 3)))), Num(3))

    assertEquals(interp(Add(1, BindCC("k", 3))), Num(4))

    assertEquals(interp(Add(1, BindCC("k", App("k", 3)))), Num(4))

    assertEquals(interp(App(BindCC("k", App("k", Fun("_", 3))), 1840)), Num(3))

    assertEquals(interp(BindCC("k", App("k", App("k", App("k", 3))))), Num(3))

    assertEquals(interp(App(App(BindCC("k", "k"), Fun("x", "x")), 3)), Num(3))
  }

}
