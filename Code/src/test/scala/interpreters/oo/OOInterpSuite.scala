package interpreters.oo

import interpreters.oo.ObjectOrientation.*

import scala.language.implicitConversions

class OOInterpSuite extends munit.FunSuite {

  import Basic.*
  val dummyClass: Class = Class(Nil, Map.empty)
  val newDummy: New     = New("Dummy", Nil)
  val dummyVal: Object  = Object("Dummy", Nil)

  val altDummyClass: Class = Class(Nil, Map.empty)
  val newAltDummy: New     = New("AltDummy", Nil)
  val altDummyVal: Object  = Object("AltDummy", Nil)

  val testclassesBase: Map[String, Class] = Map("Dummy" -> dummyClass, "AltDummy" -> altDummyClass)

  def eval(testclasses: Map[String, Class], e: OOExpr): Value =
    interp(e, Map.empty, testclassesBase ++ testclasses)

  implicit def symbolToExpr(s: String): OOExpr = Id(s)

  test("method call") {
    val testclasses = Map("Foo" -> Class(Nil, Map("foo" -> Method(Nil, newDummy))))
    val result      = eval(testclasses, Call(New("Foo", Nil), "foo", Nil))
    assertEquals(result, dummyVal)
  }

  test("method call with multiple methods") {
    val fooClass = Class(
      Nil,
      Map(
        "bar"  -> Method(Nil, Id("error")),
        "baz"  -> Method(Nil, Id("error")),
        "foo"  -> Method(Nil, newDummy),
        "zort" -> Method(Nil, Id("error"))
      )
    )
    val testclasses = Map("Foo" -> fooClass)
    val result      = eval(testclasses, Call(New("Foo", Nil), "foo", Nil))
    assertEquals(result, dummyVal)
  }

  test("method call with argument access") {
    val testclasses = Map("Foo" -> Class(Nil, Map("foo" -> Method(List("x"), Id("x")))))
    val result      = eval(testclasses, Call(New("Foo", Nil), "foo", List(newDummy)))
    assertEquals(result, dummyVal)
  }

  test("method call with multiple arguments") {
    val testclasses = Map("Foo" -> Class(
      Nil,
      Map(
        "foo" -> Method(List("x", "y", "z"), Id("y"))
      )
    ))
    val result = eval(testclasses, Call(New("Foo", Nil), "foo", List(newAltDummy, newDummy, newAltDummy)))
    assertEquals(result, dummyVal)
  }

  test("method call with this access") {
    val testclasses = Map("Foo" -> Class(Nil, Map("foo" -> Method(Nil, Id("this")))))
    val result      = eval(testclasses, Call(New("Foo", Nil), "foo", Nil))
    assertEquals(result, Object("Foo", Nil))
  }

  test("method call with field access") {
    val testclasses = Map("Foo" -> Class(List("bar"), Map("foo" -> Method(Nil, GetField("this", "bar")))))
    val result      = eval(testclasses, Call(New("Foo", List(newDummy)), "foo", Nil))
    assertEquals(result, dummyVal)
  }

  test("set and get field") {
    val testclasses = Map("Foo" -> Class(List("bar"), Map.empty))
    val result      = eval(testclasses, GetField(New("Foo", List(newDummy)), "bar"))
    assertEquals(result, dummyVal)
  }

  test("set and get with multiple fields") {
    val testclasses = Map("Foo" -> Class(List("bar", "foo", "baz"), Map.empty))
    val result      = eval(testclasses, GetField(New("Foo", List(newAltDummy, newDummy, newAltDummy)), "foo"))
    assertEquals(result, dummyVal)
  }

  test("set and get field with name 'this'") {
    val testclasses = Map("Foo" -> Class(List("this"), Map.empty))
    val result      = eval(testclasses, GetField(New("Foo", List(newDummy)), "this"))
    assertEquals(result, dummyVal)
  }

  test("field does not shadow this") {
    val testclasses = Map("Foo" -> Class(List("this"), Map("foo" -> Method(Nil, Id("this")))))
    val result      = eval(testclasses, Call(New("Foo", List(newDummy)), "foo", Nil))
    assertEquals(result, Object("Foo", List(dummyVal)))
  }

  test("arg does not shadow this") {
    val testclasses = Map("Foo" -> Class(Nil, Map("foo" -> Method(List("this"), Id("this")))))
    val result      = eval(testclasses, Call(New("Foo", Nil), "foo", List(newDummy)))
    assertEquals(result, Object("Foo", Nil))
  }

  test("arg shadows field") {
    val testclasses = Map("Foo" -> Class(List("bar"), Map("foo" -> Method(List("bar"), Id("bar")))))
    val result      = eval(testclasses, Call(New("Foo", List(newAltDummy)), "foo", List(newDummy)))
    assertEquals(result, dummyVal)
  }
}
