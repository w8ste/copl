package interpreters.oo

class OOTests extends munit.FunSuite {

  import ObjectOrientation.*
  import Basic.*

  val testclasses: Map[String, Class] = Map(
    "True"  -> Class(Nil, Map("and" -> Method(List("other"), Id("other")))),
    "False" -> Class(Nil, Map("and" -> Method(List("other"), Id("this")))),
    "Box"   -> Class(List("v"), Map("get" -> Method(Nil, GetField(Id("this"), "v"))))
  )

  val tE: New    = New("True", Nil)
  val tV: Object = Object("True", Nil)
  val fE: New    = New("False", Nil)
  val fV: Object = Object("False", Nil)

  def interp(e: OOExpr): Value = ObjectOrientation.Basic.interp(e, Map(), testclasses)

  test("all") {

    assertEquals(interp(Call(New("Box", List(fE)), "get", Nil)), fV)
    assertEquals(interp(Call(tE, "and", List(tE))), tV)
    assertEquals(interp(Call(fE, "and", List(tE))), fV)
  }

}
