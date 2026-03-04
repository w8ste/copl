package homework.assignment06state

import homework.assignment06state.Task4Arrays.*

import scala.language.implicitConversions

class Task4ArraysTest extends munit.FunSuite {

  import scala.language.implicitConversions

  implicit def idToSCFLAE(id: String): ASCFLAE = Id(id)
  implicit def numToSCFLAE(n: Int): ASCFLAE    = Num(n)

  test("box-like storage") {
    assertEquals(
      {
        interp(
          Let(
            "array",
            NewArray(1),
            Seqn(
              SetArrayIndex("array", 0, 1),
              GetArrayIndex("array", 0)
            )
          )
        )._1
      },
      Num(1)
    )
  }

  test("array storage") {
    assertEquals(
      {
        interp(
          Let(
            "array",
            NewArray(3),
            Seqn(
              Seqn(
                Seqn(
                  SetArrayIndex("array", 0, Add(1, 2)),
                  SetArrayIndex("array", 1, 2)
                ),
                SetArrayIndex("array", 2, 1)
              ),
              Add(
                Add(
                  GetArrayIndex("array", 1),
                  GetArrayIndex("array", 2)
                ),
                GetArrayIndex("array", 0)
              )
            )
          )
        )._1
      },
      Num(6)
    )
  }

  test("array store initialization") {
    val store = interp(NewArray(3))._2
    assertEquals(store.size, 3)
    assertEquals(store.values.forall(_ == Undefined), true)
  }

  test("set multiple times") {
    assertEquals(
      {
        interp(
          Let(
            "array",
            NewArray(3),
            Seqn(
              Seqn(
                Seqn(
                  SetArrayIndex("array", 0, 3),
                  SetArrayIndex("array", 0, 2)
                ),
                SetArrayIndex("array", 0, 1)
              ),
              GetArrayIndex("array", 0)
            )
          )
        )._1
      },
      Num(1)
    )
  }

  test("complex index/size expressions") {
    assertEquals(
      {
        interp(
          Let(
            "f",
            Fun("x", Add("x", 1)),
            Let(
              "array",
              NewArray(App("f", 2)),
              Seqn(
                Seqn(
                  Seqn(
                    SetArrayIndex("array", App("f", -1), Add(1, 2)),
                    SetArrayIndex("array", Add(0, 1), 2)
                  ),
                  SetArrayIndex("array", 2, 1)
                ),
                Add(
                  Add(
                    GetArrayIndex("array", App("f", 0)),
                    GetArrayIndex("array", Add(1, 1))
                  ),
                  GetArrayIndex("array", 0)
                )
              )
            )
          )
        )._1
      },
      Num(6)
    )
  }

  test("invalid size") {
    intercept[RuntimeException] {
      interp(
        Let(
          "array",
          NewArray(Fun("x", "x")),
          Seqn(
            SetArrayIndex("array", 0, 1),
            GetArrayIndex("array", 0)
          )
        )
      )._1
    }
  }

  test("zero size") {
    intercept[RuntimeException] {
      interp(Let("array", NewArray(0), 0))
    }
  }

  test("out-of-bounds get-index") {
    intercept[RuntimeException] {
      interp(Seqn(
        NewArray(5),
        Let(
          "array",
          NewArray(3),
          Seqn(
            NewArray(5),
            Seqn(
              SetArrayIndex("array", 0, 1),
              GetArrayIndex("array", 3)
            )
          )
        )
      ))
    }
  }

  test("nested arrays") {
    assertEquals(
      {
        interp(
          Let(
            "array",
            NewArray(2),
            Let(
              "array2",
              NewArray(2),
              Seqn(
                Seqn(
                  Seqn(
                    SetArrayIndex("array", 0, "array2"),
                    SetArrayIndex("array", 1, 1)
                  ),
                  SetArrayIndex("array2", 1, 2)
                ),
                Add(
                  GetArrayIndex("array", 1),
                  GetArrayIndex(GetArrayIndex("array", 0), 1)
                )
              )
            )
          )
        )._1
      },
      Num(3)
    )
  }
  test("complex index/size expressions 2") {
    assertEquals(
      {
        interp(
          Let(
            "index0",
            0,
            Let(
              "index1",
              1,
              Let(
                "array",
                NewArray(2),
                Seqn(
                  Seqn(
                    Seqn(
                      SetArrayIndex("array", "index0", 1),
                      SetArrayIndex("array", "index1", 2)
                    ),
                    SetArrayIndex("array", "index0", 3)
                  ),
                  Add(
                    GetArrayIndex("array", "index0"),
                    GetArrayIndex("array", "index1")
                  )
                )
              )
            )
          )
        )._1
      },
      Num(5)
    )
  }
  test("store propagation") {
    val adder = SetArrayIndex("array", 0, Add(GetArrayIndex("array", 0), 1))
    assertEquals(
      {
        interp(
          Let(
            "array",
            NewArray(1),
            Seqn(
              Seqn(
                SetArrayIndex("array", 0, 0),
                SetArrayIndex(
                  Seqn(adder, "array"),
                  Seqn(adder, 0),
                  Seqn(adder, Add(GetArrayIndex("array", 0), 1))
                )
              ),
              GetArrayIndex(
                Seqn(adder, "array"),
                Seqn(adder, 0)
              )
            )
          )
        )._1
      },
      Num(6)
    )
  }
  test("evaluation order 1") {
    assertEquals(
      {
        interp(
          Let(
            "array",
            NewArray(1),
            Seqn(
              SetArrayIndex("array", Seqn(SetArrayIndex("array", 0, 1), 0), 2),
              GetArrayIndex("array", 0)
            )
          )
        )._1
      },
      Num(2)
    )
  }
  test("evaluation order 2") {
    assertEquals(
      {
        interp(
          Let(
            "array",
            NewArray(2),
            Seqn(
              SetArrayIndex(Seqn(SetArrayIndex("array", 0, 1), "array"), 0, 2),
              GetArrayIndex("array", 0)
            )
          )
        )._1
      },
      Num(2)
    )
  }
  test("evaluation order 3") {
    assertEquals(
      {
        interp(
          Let(
            "array",
            NewArray(2),
            Seqn(
              SetArrayIndex("array", 0, Seqn(SetArrayIndex("array", 0, 1), 2)),
              GetArrayIndex("array", 0)
            )
          )
        )._1
      },
      Num(2)
    )
  }
  test("invalid set index") {
    intercept[RuntimeException] {
      interp(
        Let(
          "array",
          NewArray(1),
          Seqn(
            SetArrayIndex("array", Fun("x", "x"), 1),
            GetArrayIndex("array", 0)
          )
        )
      )._1
    }
  }
  test("invalid get index") {
    intercept[RuntimeException] {
      interp(
        Let(
          "array",
          NewArray(1),
          Seqn(
            SetArrayIndex("array", 0, 1),
            GetArrayIndex("array", Fun("x", "x"))
          )
        )
      )._1
    }
  }
  test("undefined values") {
    assertEquals(
      {
        interp(Let(
          "array",
          NewArray(3),
          Seqn(
            SetArrayIndex("array", 0, 1),
            GetArrayIndex("array", 1)
          )
        ))._1
      },
      Undefined
    )
  }

  test("negative size") {
    intercept[RuntimeException] {
      interp(Let("array", NewArray(-1), 0))
    }
  }

  test("negative set-index") {
    intercept[RuntimeException] {
      interp(Seqn(
        NewArray(5),
        Let(
          "array",
          NewArray(3),
          Seqn(
            NewArray(5),
            Seqn(
              SetArrayIndex("array", -1, 1),
              GetArrayIndex("array", 0)
            )
          )
        )
      ))
    }
  }

  test("out-of-bounds set-index") {
    intercept[RuntimeException] {
      interp(Seqn(
        NewArray(5),
        Let(
          "array",
          NewArray(3),
          Seqn(
            NewArray(5),
            Seqn(
              SetArrayIndex("array", 3, 1),
              GetArrayIndex("array", 0)
            )
          )
        )
      ))
    }
  }

  test("negative get-index") {
    intercept[RuntimeException] {
      interp(Seqn(
        NewArray(5),
        Let(
          "array",
          NewArray(3),
          Seqn(
            NewArray(5),
            Seqn(
              SetArrayIndex("array", 0, 1),
              GetArrayIndex("array", -1)
            )
          )
        )
      ))
    }
  }

  // returns 2 if array location is not taken into account when reading/writing
  test("get set uses correct array offset") {
    assertEquals(
      {
        interp(
          Let(
            "arr1",
            NewArray(3),
            Let(
              "arr2",
              NewArray(3),
              Seqn(
                Seqn(
                  SetArrayIndex("arr2", 0, 1),
                  SetArrayIndex("arr1", 0, 2)
                ),
                GetArrayIndex("arr2", 0)
              )
            )
          )
        )._1
      },
      Num(1)
    )
  }
}
