package homework.assignment00hello

import homework.assignment00hello.PeanoNumbers.*
import munit.FunSuite

class PeanoNumbersTests extends FunSuite {

  val one: Succ   = Succ(Zero)
  val two: Succ   = Succ(Succ(Zero))
  val three: Succ = Succ(Succ(Succ(Zero)))
  val five: Succ  = Succ(Succ(Succ(Succ(Succ(Zero)))))

  test("add one zero") { assertEquals(add(one, Zero), one) }
  test("add zero five") { assertEquals(add(Zero, five), five) }
  test("add one one") { assertEquals(add(one, one), two) }
  test("add two three") { assertEquals(add(two, three), five) }

}
