package interpreters.oo

class OOWithInheritanceTests extends munit.FunSuite {

  import ObjectOrientation.*
  import Inheritance.*

  val testclasses: Map[String, Class] = Map(
    "True" -> Class(
      "Object",
      List.empty,
      Map(
        "ifThenElse" -> Method(List("thenExp", "elseExp"), Id("thenExp")),
        "and"        -> Method(List("x"), Id("x"))
      )
    ),
    "False" -> Class(
      "Object",
      List.empty,
      Map(
        "ifThenElse" -> Method(List("thenExp", "elseExp"), Id("elseExp")),
        "and"        -> Method(List("x"), Id("this"))
      )
    ),
    "Food" -> Class(
      "Object",
      List("organic"),
      Map(
        "tastesBetterThan" ->
        Method(
          List("other"),
          Call(
            GetField(Id("this"), "organic"),
            "ifThenElse",
            List(
              New("True", List.empty),
              GetField(Id("otherFood"), "organic")
            )
          )
        )
      )
    ),
    "Pizza" -> Class(
      "Food",
      List("hasCheese"),
      Map(
        "tastesBetterThan" ->
        Method(
          List("other"),
          Call(
            GetField(Id("this"), "organic"),
            "and",
            List(GetField(Id("this"), "hasCheese"))
          )
        )
      )
    )
  )

  test("Weird made up subclass example test") {

    val testRes =
      interp(
        Call(
          New("Pizza", List(New("True", List.empty), New("True", List.empty))),
          "tastesBetterThan",
          List(New("Food", List(New("True", List.empty))))
        ),
        Map(),
        testclasses
      )

    assertEquals(
      testRes,
      Object("True", List.empty)
    )
  }

  test("Weird made up subclass example test 2") {

    val testRes =
      interp(
        Call(
          New("Pizza", List(New("False", List.empty), New("True", List.empty))),
          "tastesBetterThan",
          List(New("Food", List(New("True", List.empty))))
        ),
        Map(),
        testclasses
      )
    assertEquals(
      testRes,
      Object("False", List.empty)
    )
  }

  test("Weird made up subclass example test 3") {

    val testRes =
      interp(
        GetField(
          New("Pizza", List(New("False", List.empty), New("True", List.empty))),
          "organic"
        ),
        Map(),
        testclasses
      )
    assertEquals(
      testRes,
      Object("False", List.empty)
    )
  }

}
