package modularized

import modularized.continuations.Continuations.*

import scala.util.boundary

class ContinuationsTest extends munit.FunSuite {

  test("basic addition") {
    val result = boundary {
      interp(Add(Num(2), Num(3)), Map.empty).apply(boundary.break(_))
    }
    assertEquals(result, Num(5)) // Assuming Num is the type for numeric values
  }

  test("basic addition with bindCC") {
    val result = boundary {
      interp(Add(Num(2), BindCC("k", Num(3))), Map.empty).apply(boundary.break(_))
    }
    assertEquals(result, Num(5)) // Assuming Num is the type for numeric values
  }

  test("basic addition using bindCC") {
    val result = boundary {
      interp(Add(Num(2), BindCC("k", Add(Num(1), AppK(Id("k"), Num(3))))), Map.empty).apply(boundary.break(_))
    }
    assertEquals(result, Num(5)) // Assuming Num is the type for numeric values
  }
}
