package interpreters.conditionals

class ConditionalsStaticFirstOrderTests extends munit.FunSuite {

  import ConditionalsStaticFirstOrder.*

  import scala.language.implicitConversions

  implicit def symbolToF1LAE(symbol: String): Id = Id(symbol)
  implicit def intToF1LAE(n: Int): Num           = Num(n)

  test("all") {

    val funDefs3 = Map("fact" -> FunDef("n", If0("n", 1, Mult("n", App("fact", Sub("n", 1))))))
    val res      = interp(App("fact", 5), funDefs3, Map())
    println(res)

    val funDefs = Map(
      "f" -> FunDef("n", App("g", Add("n", 5))),
      "g" -> FunDef("n", Sub("n", 1))
    )

    assertEquals(interp(App("f", 5), funDefs, Map()), 9)

    val funDefs2 = Map(
      "f" -> FunDef("y", Sub("y", 1)),
      "g" -> FunDef("y", Sub("y", 1)),
      "f" -> FunDef("x", App("g", Add("x", 3)))
    )

    assertEquals(interp(App("f", 10), funDefs2, Map()), 12)

    assertEquals(
      interp(
        Let("x", 3, Add(Let("x", 4, Add("x", 3)), "x")),
        Map(),
        Map()
      ),
      10
    )

    intercept[Exception](interp(Let("n", 5, App("f", 10)), Map("f" -> FunDef("x", "n")), Map()))

    intercept[Exception](interp(App("f", 4), Map("f" -> FunDef("y", Add("x", "y"))), Map()))

  }

}
