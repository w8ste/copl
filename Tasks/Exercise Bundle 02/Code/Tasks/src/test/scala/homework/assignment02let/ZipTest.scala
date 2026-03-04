package homework.assignment02let

import homework.assignment02let.Zip.*

class ZipTest extends munit.FunSuite {
  test("Zipadd0") {
    assertEquals(zipadd(List(1, 2, 3), List(4, 5, 6)), List(5, 7, 9))
  }

  test("Zipadd1") {
    assertEquals(zipadd(List(9, 8, 4, 3, 6), List(8, 4, 6, 5, 1)), List(17, 12, 10, 8, 7))
  }

  test("Zipadd2") {
    assertEquals(zipadd(List(9, 8, 4, 3, 6), List(8, 4, 6)), List(17, 12, 10))
  }

  test("Zipadd3") {
    assertEquals(zipadd(List(9, 8), List(8, 4, 6, 5, 1)), List(17, 12))
  }

  test("Zip0") {
    assertEquals(zip(List(1, 2, 3), List(4, 5, 6)), List((1, 4), (2, 5), (3, 6)))
  }

  test("Zip1") {
    assertEquals(zip(List(9, 8, 4, 3, 6), List(8, 4, 6, 5, 1)), List((9, 8), (8, 4), (4, 6), (3, 5), (6, 1)))
  }

  test("Zip2") {
    assertEquals(zip(List(9, 8, 4, 3, 6), List(8, 4, 6, 5)), List((9, 8), (8, 4), (4, 6), (3, 5)))
  }

  test("Zip3") {
    assertEquals(zip(List(9), List(8, 4, 6, 5, 1)), List((9, 8)))
  }

  test("AddMatrix0") {
    assertEquals(
      addMatrix(List(List(1, 2), List(4, 5)), List(List(7, 8), List(9, 10))),
      List(List(8, 10), List(13, 15))
    )
  }

  test("AddMatrix1") {
    assertEquals(
      addMatrix(List(List(9, 8, 4), List(3, 6, 8), List(4, 6, 5)), List(List(1, 3, 6), List(1, 7, 6), List(7, 6, 1))),
      List(List(10, 11, 10), List(4, 13, 14), List(11, 12, 6))
    )
  }

  test("AddMatrix2") {
    assertEquals(
      addMatrix(List(List(9, 8, 4), List(3, 6), List(4, 6, 5)), List(List(1, 3, 6), List(1, 7, 6), List(7))),
      List(List(10, 11, 10), List(4, 13), List(11))
    )
  }

  test("AddMatrix3") {
    assertEquals(
      addMatrix(List(List(9), List(3, 6, 8), List(4, 6, 5)), List(List(1, 3, 6), List(1, 7, 6))),
      List(List(10), List(4, 13, 14))
    )
  }
}
