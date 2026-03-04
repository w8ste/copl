package homework.assignment08objectstate

import homework.assignment08objectstate.MutableObjects.*
import interpreters.memory.RootedEnv
import interpreters.memory.stores.MinimalStore

import scala.language.implicitConversions

class MutableObjectsTest extends munit.FunSuite {
  val dummyClass: Class = Class("Object", Nil, Map.empty)
  val newDummy: New     = New("Dummy")
  val dummyVal: Object  = Object("Dummy", Nil)

  val altDummyClass: Class = Class("Object", Nil, Map.empty)
  val newAltDummy: New     = New("AltDummy")
  val altDummyVal: Object  = Object("AltDummy", Nil)

  val testclassesBase: Map[String, Class] = Map(
    "Dummy"    -> dummyClass,
    "AltDummy" -> altDummyClass
  )

  def eval(testclasses: Map[String, Class], e: OOExpr): Value = {
    val (v, s) = evalStore(testclasses, e)
    v
  }

  def evalStore(testclasses: Map[String, Class], e: OOExpr): (Value, MinimalStore[Value]) = {
    val store  = new MinimalStore[Value](100)
    val (v, s) = interp(e, RootedEnv.empty, store, testclassesBase ++ testclasses)
    (v, s.asInstanceOf[MinimalStore[Value]])
  }

  implicit def symbolToExpr(s: String): OOExpr = Id(s)
  implicit def numToExpr(n: Int): OOExpr       = Num(n)

  test("empty seqn") {
    intercept[Exception](eval(Map.empty, Seqn()))
  }

  test("method call") {
    val testclasses = Map(
      "Foo" -> Class("Object", Nil, Map("foo" -> Method(newDummy)))
    )
    val result = eval(testclasses, Call(New("Foo"), "foo"))
    assertEquals(result, dummyVal)
  }

  test("method call with multiple methods") {
    val fooClass = Class(
      "Object",
      Nil,
      Map(
        "bar"  -> Method("error"),
        "baz"  -> Method("error"),
        "foo"  -> Method(newDummy),
        "zort" -> Method("error")
      )
    )
    val testclasses = Map("Foo" -> fooClass)
    val result      = eval(testclasses, Call(New("Foo"), "foo"))
    assertEquals(result, dummyVal)
  }

  test("method call with argument access") {
    val testclasses = Map("Foo" -> Class("Object", Nil, Map("foo" -> Method("x", "x"))))
    val result      = eval(testclasses, Call(New("Foo"), "foo", newDummy))
    assertEquals(result, dummyVal)
  }

  test("method call with multiple arguments") {
    val testclasses = Map("Foo" -> Class(
      "Object",
      Nil,
      Map(
        "foo" -> Method("y", "x", "y", "z")
      )
    ))
    val result = eval(testclasses, Call(New("Foo"), "foo", newAltDummy, newDummy, newAltDummy))
    assertEquals(result, dummyVal)
  }

  test("method call with this access") {
    val testclasses = Map("Foo" -> Class("Object", Nil, Map("foo" -> Method("this"))))
    val result      = eval(testclasses, Call(New("Foo"), "foo"))
    assertEquals(result, Object("Foo", Nil))
  }

  test("get field") {
    val testclasses = Map("Foo" -> Class("Object", List("bar"), Map.empty))
    val result      = eval(testclasses, GetField(New("Foo", newDummy), "bar"))
    assertEquals(result, dummyVal)
  }

  test("get with multiple fields") {
    val testclasses = Map(
      "Foo" -> Class("Object", List("bar", "foo", "baz"), Map.empty)
    )
    val result = eval(testclasses, GetField(New("Foo", newAltDummy, newDummy, newAltDummy), "foo"))
    assertEquals(result, dummyVal)
  }

  test("get field with name 'this'") {
    val testclasses = Map("Foo" -> Class("Object", List("this"), Map.empty))
    val result      = eval(testclasses, GetField(New("Foo", newDummy), "this"))
    assertEquals(result, dummyVal)
  }

  test("field does not shadow this") {
    val testclasses = Map(
      "Foo" -> Class("Object", List("this"), Map("foo" -> Method("this")))
    )
    val (result, store) = evalStore(testclasses, Call(New("Foo", newDummy), "foo"))
    result match {
      case Object(name, fLocs) =>
        assertEquals(dummyVal, store.lookup(fLocs(0)))
      case _ => fail("incorrect shadow")
    }
  }

  test("arg does not shadow this") {
    val testclasses = Map(
      "Foo" -> Class("Object", Nil, Map("foo" -> Method("this", "this")))
    )
    val result = eval(testclasses, Call(New("Foo"), "foo", newDummy))
    assertEquals(result, Object("Foo", Nil))
  }

  test("arg shadows field") {
    val testclasses = Map(
      "Foo" -> Class("Object", List("bar"), Map("foo" -> Method("bar", "bar")))
    )
    val result = eval(testclasses, Call(New("Foo", newAltDummy), "foo", newDummy))
    assertEquals(result, dummyVal)
  }

  test("get field from parent") {
    val testclasses = Map(
      "Foo" -> Class("Object", List("field"), Map.empty),
      "Bar" -> Class("Foo", Nil, Map.empty)
    )
    val result = eval(testclasses, GetField(New("Bar", newDummy), "field"))
    assertEquals(result, dummyVal)
  }

  test("parent fields before child fields") {
    val testclasses = Map(
      "Foo" -> Class("Object", List("pField"), Map.empty),
      "Bar" -> Class("Foo", List("cField"), Map.empty)
    )
    val result = eval(testclasses, GetField(New("Bar", newDummy, newAltDummy), "pField"))
    assertEquals(result, dummyVal)
  }

  test("access methods from parent") {
    val testclasses = Map(
      "Foo" -> Class("Object", Nil, Map("foo" -> Method(newDummy))),
      "Bar" -> Class("Foo", Nil, Map.empty)
    )
    val result = eval(testclasses, Call(New("Bar"), "foo"))
    assertEquals(result, dummyVal)
  }

  test("access methods from ancestor") {
    val testclasses = Map(
      "Foo" -> Class("Object", Nil, Map("foo" -> Method(newDummy))),
      "Bar" -> Class("Foo", Nil, Map.empty),
      "Baz" -> Class("Foo", Nil, Map.empty)
    )
    val result = eval(testclasses, Call(New("Baz"), "foo"))
    assertEquals(result, dummyVal)
  }

  test("does not override field in child") {
    val testclasses = Map(
      "Foo" -> Class("Object", List("pField"), Map.empty),
      "Bar" -> Class("Foo", List("pField", "cField"), Map.empty)
    )
    val result = eval(testclasses, GetField(New("Bar", newAltDummy, newDummy, newAltDummy), "cField"))
    assertEquals(result, altDummyVal)
  }

  test("child field shadows parent field") {
    val testclasses = Map(
      "Foo" -> Class("Object", List("field"), Map.empty),
      "Bar" -> Class("Foo", List("field"), Map.empty)
    )
    val result = eval(testclasses, GetField(New("Bar", newAltDummy, newDummy), "field"))
    assertEquals(result, dummyVal)
  }

  test("override methods in child") {
    val testclasses = Map(
      "Foo" -> Class("Object", Nil, Map("foo" -> Method(newDummy))),
      "Bar" -> Class("Foo", Nil, Map("foo" -> Method(newAltDummy)))
    )
    val result = eval(testclasses, Call(New("Bar"), "foo"))
    assertEquals(result, altDummyVal)
  }

  test("override methods in grandchild") {
    val testclasses = Map(
      "Foo" -> Class("Object", Nil, Map("foo" -> Method(newDummy))),
      "Bar" -> Class("Foo", Nil, Map.empty),
      "Baz" -> Class("Foo", Nil, Map("foo" -> Method(newAltDummy)))
    )
    val result = eval(testclasses, Call(New("Baz"), "foo"))
    assertEquals(result, altDummyVal)
  }

  test("mutable field with SetField") {
    val testclasses = Map(
      "Foo" -> Class("Object", List("field"), Map.empty)
    )
    val result = eval(
      testclasses,
      Let(
        "foo",
        New("Foo", newDummy),
        Seqn(
          SetField("foo", "field", newAltDummy),
          GetField("foo", "field")
        )
      )
    )
    assertEquals(result, altDummyVal)
  }

  test("mutable field in method call") {
    val testclasses = Map(
      "Foo" -> Class(
        "Object",
        List("field"),
        Map("set" -> Method(SetField("this", "field", "x"), "x"))
      )
    )
    val result = eval(
      testclasses,
      Let(
        "foo",
        New("Foo", newDummy),
        Seqn(
          Call("foo", "set", newAltDummy),
          GetField("foo", "field")
        )
      )
    )
    assertEquals(result, altDummyVal)
  }

  test("method call with direct field access") {
    val testclasses = Map(
      "Foo" -> Class("Object", List("bar"), Map("foo" -> Method("bar")))
    )
    val (result, store) = evalStore(testclasses, Call(New("Foo", newDummy), "foo"))
    println(store)
    assertEquals(result, dummyVal)
  }

  test("mutable field with direct access in method call") {
    val testclasses = Map(
      "Foo" -> Class(
        "Object",
        List("field"),
        Map("set" -> Method(SetId("field", "x"), "x"))
      )
    )
    val result = eval(
      testclasses,
      Let(
        "foo",
        New("Foo", newDummy),
        Seqn(
          Call("foo", "set", newAltDummy),
          GetField("foo", "field")
        )
      )
    )
    assertEquals(result, altDummyVal)
  }

  test("scenario 1") {
    val testclasses = Map(
      "Vehicle"   -> Class("Object", List("speed"), Map.empty),
      "Motorized" -> Class(
        "Vehicle",
        List("power"),
        Map(
          "accel" -> Method(SetId("speed", Add("speed", Mult("s", "power"))), "s")
        )
      ),
      "Transport" -> Class("Motorized", List("seats"), Map.empty)
    )
    val result = eval(
      testclasses,
      Let(
        "bus",
        New("Transport", 0, 5, 32),
        Seqn(
          Call("bus", "accel", 2),
          SetField("bus", "power", 3),
          Call("bus", "accel", 2),
          GetField("bus", "speed")
        )
      )
    )
    assertEquals(result, Num(16))
  }

  test("scenario 2") {
    val testclasses = Map(
      "Vehicle" -> Class(
        "Object",
        List("speed"),
        Map(
          "speed" -> Method("speed")
        )
      ),
      "Motorized" -> Class(
        "Vehicle",
        List("power"),
        Map(
          "accel" -> Method(SetId("speed", Add("speed", Mult("s", "power"))), "s")
        )
      ),
      "Transport" -> Class("Motorized", List("seats"), Map.empty),
      "Driver"    -> Class(
        "Object",
        Nil,
        Map(
          "drive" -> Method(Call("motorized", "accel", 5), "motorized")
        )
      )
    )
    val result = eval(
      testclasses,
      Let(
        "bike",
        New("Transport", 0, 30, 1),
        Seqn(
          Let("driver", New("Driver"), Call("driver", "drive", "bike")),
          Call("bike", "speed")
        )
      )
    )
    assertEquals(result, Num(150))
  }

  test("scenario 3") {
    val testclasses = Map(
      "Vehicle" -> Class(
        "Object",
        List("speed"),
        Map(
          "speed"   -> Method("speed"),
          "speed_=" -> Method(SetId("speed", "x"), "x")
        )
      ),
      "Motorized" -> Class(
        "Vehicle",
        List("power"),
        Map(
          "accel" -> Method(
            Call("this", "speed_=", Add("speed", Mult("s", "power"))),
            "s"
          )
        )
      ),
      "Transport" -> Class("Motorized", List("seats"), Map.empty),
      "Driver"    -> Class(
        "Object",
        Nil,
        Map(
          "drive" -> Method(Call("motorized", "accel", 5), "motorized")
        )
      )
    )
    val result = eval(
      testclasses,
      Let(
        "bike",
        New("Transport", 0, 30, 1),
        Seqn(
          Let(
            "driver",
            New("Driver"),
            Seqn(
              Call("driver", "drive", "bike"),
              Call("driver", "drive", "bike")
            )
          ),
          Call("bike", "speed")
        )
      )
    )
    assertEquals(result, Num(300))
  }

  test("scenario 4") {
    val testclasses = Map(
      "Vehicle" -> Class(
        "Object",
        List("speed"),
        Map(
          "speed"   -> Method("speed"),
          "speed_=" -> Method(SetId("speed", "x"), "x")
        )
      ),
      "Motorized" -> Class(
        "Vehicle",
        List("power"),
        Map(
          "accel" -> Method(
            Call("this", "speed_=", Add("speed", Mult("s", "power"))),
            "s"
          )
        )
      ),
      "Transport" -> Class("Motorized", List("seats"), Map.empty),
      "Driver"    -> Class(
        "Object",
        Nil,
        Map(
          "drive" -> Method(Call("motorized", "accel", 5), "motorized")
        )
      ),
      "LongDriver" -> Class(
        "Object",
        Nil,
        Map(
          "drive" -> Method(Call("motorized", "accel", 10), "motorized")
        )
      )
    )
    val result = eval(
      testclasses,
      Let(
        "bike",
        New("Transport", 0, 30, 1),
        Seqn(
          Let(
            "driver",
            New("Driver"),
            Seqn(
              Call("driver", "drive", "bike"),
              SetId("driver", New("LongDriver")),
              Call("driver", "drive", "bike")
            )
          ),
          Call("bike", "speed")
        )
      )
    )
    assertEquals(result, Num(450))
  }
}
