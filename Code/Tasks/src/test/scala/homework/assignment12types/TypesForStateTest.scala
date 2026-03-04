package homework.assignment12types

import homework.assignment12types.TypesForState.StateWithRecords
import homework.assignment12types.TypesForState.StateWithRecords.*

import scala.language.implicitConversions

class TypesForStateTest extends munit.FunSuite {

  implicit def idToSFLAERec(id: String): Id = Id(id)
  implicit def numToSFLAERec(n: Int): Num   = Num(n)

  def typeOf(e: Expr): Type = StateWithRecords.typeOf(e, Map.empty)

  val record: Record  = Record(List("a" -> 1, "b" -> 2))
  val makeRecord: Fun = Fun("val", TNum(), Record(List("key" -> "val", "key2" -> Add("val", 1))))
  val nested: Record  = Record(List("key" -> App(makeRecord, 1)))

  // An object that mutates a global variable "b" whenever we instantiate it
  // (the global variable "b" of course has to be initialized before using this instance!).
  // Whenever we use this record in the code,
  // the fields will be fully evaluated (from left to right).
  // You can roughly think of the fields of the record below as "val" fields of objects in Scala,
  // which will also be evaluated completely as soon as the object is initialized.
  val mutateB: Record = Record(List(
    "addOne" -> SetBox("b", Add(OpenBox("b"), 1)),
    "double" -> SetBox("b", Add(OpenBox("b"), OpenBox("b")))
  ))

  // We can also use Records with state to create mutable objects, i.e. objects whose
  // fields we can mutate "upon request" (i.e. by record projection).
  // To achieve this, we wrap the record fields in function definitions (and ignore the function argument):
  val counter: Record = Record(List(
    "get" -> Fun("x", TNum(), OpenBox("c")),
    "inc" -> Fun("x", TNum(), SetBox("c", Add(OpenBox("c"), 1)))
  ))

  // We can create a constructor for a counter object like this:
  val constructcounter: Fun = Fun("x", TNum(), Let("c", NewBox("x"), counter))

  test("1") {
    assertEquals(typeOf(record), TRecord(List("a" -> TNum(), "b" -> TNum())))
  }

  test("2") {
    assertEquals(typeOf(App(makeRecord, 1)), TRecord(List("key" -> TNum(), "key2" -> TNum())))
  }

  test("3") {
    assertEquals(typeOf(RecordProjection(record, "b")), TNum())
  }

  test("4") {
    assertEquals(typeOf(Add(RecordProjection(record, "a"), RecordProjection(record, "b"))), TNum())
  }

  test("5") {
    assertEquals(
      typeOf(Add(RecordProjection(App(makeRecord, 2), "key"), RecordProjection(App(makeRecord, 3), "key2"))),
      TNum()
    )
  }

  test("6") {
    assertEquals(typeOf(RecordProjection(RecordProjection(nested, "key"), "key2")), TNum())
  }

  test("7") {
    assertEquals(
      typeOf(Let(
        "b",
        NewBox(0),
        Seqn(
          Seqn(
            mutateB,
            mutateB
          ),
          OpenBox(Id("b"))
        )
      )),
      TNum()
    )
  }

  test("8") {
    intercept[RuntimeException] {
      typeOf(RecordProjection(record, "c"))
    }
  }

  test("9") {
    assertEquals(
      typeOf(Let(
        "mycounter",
        App(constructcounter, 42),
        Seqn(
          App(RecordProjection("mycounter", "inc"), 0),
          Seqn(
            App(RecordProjection("mycounter", "inc"), 0),
            App(RecordProjection("mycounter", "get"), 0)
          )
        )
      )),
      TNum()
    )
  }

  test("10") {
    assertEquals(typeOf(makeRecord), TFun(TNum(), TRecord(List("key" -> TNum(), "key2" -> TNum()))))
  }

  test("11") {
    assertEquals(typeOf(nested), TRecord(List("key" -> TRecord(List("key" -> TNum(), "key2" -> TNum())))))
  }

  test("12") {
    assertEquals(typeOf(NewBox(Fun("x", TNum(), "x"))), TBox(TFun(TNum(), TNum())))
  }

  test("13") {
    intercept[RuntimeException] {
      typeOf(SetBox(NewBox(Add(1, 2)), Fun("x", TNum(), "x")))
    }
  }

  test("14") {
    assertEquals(typeOf(SetBox(NewBox(Add(1, 3)), 42)), TNum())
  }

  test("15") {
    assertEquals(
      typeOf(Let(
        "x",
        NewBox(42),
        Seqn(
          SetBox("x", Add(OpenBox("x"), OpenBox("x"))),
          OpenBox("x")
        )
      )),
      TNum()
    )
  }

  test("16") {
    assertEquals(
      typeOf(Let(
        "x",
        42,
        Seqn(
          SetId("x", Add("x", "x")),
          "x"
        )
      )),
      TNum()
    )
  }

  test("16'") {
    intercept[RuntimeException] {
      typeOf(Let("x", 42, SetId("x", NewBox(7))))
    }
  }

  test("17") {
    intercept[RuntimeException] {
      typeOf(OpenBox(5))
    }
  }

  test("18") {
    intercept[RuntimeException] {
      typeOf(Let(
        "x",
        NewBox(42),
        Seqn(
          SetBox("x", Fun("y", TNum(), "y")),
          OpenBox("x")
        )
      ))
    }
  }

  test("19") {
    intercept[RuntimeException] {
      typeOf(SetBox(Fun("x", TNum(), "x"), 42))
    }
  }

  test("20") {
    assertEquals(typeOf(Fun("x", TBox(TNum()), SetBox("x", Add(OpenBox("x"), 1)))), TFun(TBox(TNum()), TNum()))
  }

}
