package homework.assignment03functions.part2

import homework.assignment03functions.part2.PreprocLet.*
import homework.assignment03functions.part2.definitions.FAEBase.*
import homework.assignment03functions.part2.definitions.FLAEBase.*
import munit.FunSuite

import scala.language.implicitConversions

class PreprocLetTest extends FunSuite {

  // implicitly convert
  //   numbers to LNums or Nums, and
  //   Symbols to LIds or Ids
  implicit def numToFLAE(n: Int): LNum      = LNum(n)
  implicit def numToFAE(n: Int): Num        = Num(n)
  implicit def symbolToFLAE(s: String): LId = LId(s)
  implicit def symbolToFAE(s: String): Id   = Id(s)

  test("num") { assertEquals(preprocLet(LNum(3)), Num(3)) }
  test("id") { assertEquals(preprocLet(LId("x")), Id("x")) }
  test("fun") { assertEquals(preprocLet(LFun("x", LId("x"))), Fun("x", Id("x"))) }
  test("let") { assertEquals(preprocLet(LLet("x", LNum(3), "x")), App(Fun("x", Id("x")), Num(3))) }

  test("add") {
    assertEquals(
      preprocLet(LAdd(LAdd(LNum(3), LNum(4)), LAdd(LNum(6), LNum(7)))),
      Add(Add(Num(3), Num(4)), Add(Num(6), Num(7)))
    )
  }

  test("sub") {
    assertEquals(
      preprocLet(LSub(LSub(LNum(3), LNum(4)), LSub(LNum(6), LNum(7)))),
      Sub(Sub(Num(3), Num(4)), Sub(Num(6), Num(7)))
    )
  }

  test("let fun") {
    assertEquals(interp(preprocLet(LLet("x", 3, LFun("y", LAdd("x", "y"))))), Fun("y", Add(3, "y")))
  }

  test("fun app") {
    assertEquals(interp(preprocLet(LApp(LLet("x", 3, LFun("y", LAdd("x", "y"))), LAdd(4, 5)))), Num(12))
  }

  test("let fun app") {
    assertEquals(
      interp(preprocLet(LLet("inc", LFun("x", LAdd("x", 1)), LAdd(LApp("inc", 4), LApp("inc", 5))))),
      Num(11)
    )
  }

  // You may write additional tests if you want... :

}
