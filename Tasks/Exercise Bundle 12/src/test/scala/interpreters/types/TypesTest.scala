package interpreters.types

class TypesTest extends munit.FunSuite {

  test("base") {
    import Base.*

    assertEquals(typeOf(Add(Num(1), Num(1)), Map.empty), TNum())

    assertEquals(
      typeOf(
        App(
          Fun("n", TNum(), Add(Id("n"), Id("n"))),
          Num(10)
        ),
        Map.empty
      ),
      TNum()
    )
  }

  test("subtype") {
    import Subtyping.*
    val recF =
      Fun(
        "x",
        TRecord(List("a" -> TNum(), "b" -> TNum())),
        Add(RecordProjection(Id("x"), "a"), RecordProjection(Id("x"), "b"))
      )

    val rec1 = Record(List("a" -> Num(1), "b" -> Num(2), "c" -> Num(3))) // too many fields
    val rec2 = Record(List("b" -> Num(1), "a" -> Num(3)))                // wrong order of fields
    val rec3 = Record(List("a" -> Num(3)))                               // too little field

    assertEquals(typeOf(App(recF, rec1), Map.empty), TNum())
    assertEquals(typeOf(App(recF, rec2), Map.empty), TNum())
    intercept[Exception](
      typeOf(App(recF, rec3), Map.empty)
    )
  }

  test("inference") {
    import Inference.*

    import scala.language.implicitConversions
    implicit def num2exp(n: Int): Expr   = Num(n)
    implicit def id2exp(s: String): Expr = Id(s)

    assertEquals(doTypeCheck(42, Map.empty), NumType)
    assertEquals(doTypeCheck(Fun("x", Add("x", 1)), Map.empty), FunType(NumType, NumType))

    // Here, we apply `id` to two different types, NumType and FunType(NumType, NumType).
    // That it does not work implies that we do not have polymorphism/generics for let (also not for functions).
    intercept[Exception](
      doTypeCheck(
        Let("id", Fun("x", "x"), App(App("id", Fun("x", Add("x", 1))), App("id", 42))),
        Map.empty
      )
    )

    // This is one of the building blocks for a fixpoint operators, which has no proper type, thus type inference fails.
    intercept[Exception](
      doTypeCheck(Fun("x", App("x", "x")), Map.empty)
    )

  }

}
