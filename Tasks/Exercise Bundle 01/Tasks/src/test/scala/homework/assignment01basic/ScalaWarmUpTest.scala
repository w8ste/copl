package homework.assignment01basic

import ScalaWarmUp.*

class ScalaWarmUpTest extends munit.FunSuite {

  test("flatten1") {assertEquals(flatten(List(List(1), List(2, 3, 4), List(5, 6))), List(1, 2, 3, 4, 5, 6))}
  test("flatten2") {assertEquals(flatten(Nil), Nil)}
  test("flatten3") {assertEquals(flatten(List(Nil)), Nil)}
  test("flatten4") {
    assertEquals(flatten(List(Nil, List(1), Nil, List(2), Nil, List(3), Nil, Nil, Nil)), List(1, 2, 3))
  }

  test("nil") {assertEquals(reverse(Nil), Nil)}
  test("singleton") {assertEquals(reverse(List(1)), List(1))}
  test("reverse") {assertEquals(reverse(List(1, 2, 3)), List(3, 2, 1))}
  test("dont use flatten for reverse") {
    assertEquals(reverse(List(List(1, 2), List(3, 4))), List(List(3, 4), List(1, 2)))
  }

}
