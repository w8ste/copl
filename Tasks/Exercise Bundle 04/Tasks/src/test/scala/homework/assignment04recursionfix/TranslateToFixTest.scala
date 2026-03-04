package homework.assignment04recursionfix

import homework.assignment04recursionfix.TranslateToFixInterpreter.*
import homework.assignment04recursionfix.TranslateToFixTask.*
import munit.FunSuite

class TranslateToFixTest extends FunSuite {

  def doesNotUseRec(e: RCFLAE): Boolean = e match {
    case Num(_)                      => true
    case Add(lhs, rhs)               => doesNotUseRec(lhs) && doesNotUseRec(rhs)
    case Sub(lhs, rhs)               => doesNotUseRec(lhs) && doesNotUseRec(rhs)
    case Mult(lhs, rhs)              => doesNotUseRec(lhs) && doesNotUseRec(rhs)
    case Let(_, namedExpr, body)     => doesNotUseRec(namedExpr) && doesNotUseRec(body)
    case Id(_)                       => true
    case Fun(_, body)                => doesNotUseRec(body)
    case If0(test, posBody, negBody) => doesNotUseRec(test) && doesNotUseRec(posBody) && doesNotUseRec(negBody)
    case LetRec(_, _, _)             => false
    case App(funExpr, argExpr)       => doesNotUseRec(funExpr) && doesNotUseRec(argExpr)
  }

  val factorial: LetRec = LetRec("fac", Fun("n", If0("n", 1, Mult("n", App("fac", Add("n", -1))))), "fac")

  test("Factorial") {
    val translatedFactorial = translate2Fix(factorial)

    assertEquals(doesNotUseRec(factorial), false)
    assertEquals(interp(App(factorial, Num(5))), Num(120))
    assertEquals(doesNotUseRec(translatedFactorial), true)

    val factorials = List((1, 1), (2, 2), (3, 6), (4, 24), (5, 120), (6, 720))
    factorials.foreach { (n: Int, r: Int) => assertEquals(interp(App(translatedFactorial, Num(n))), Num(r)) }
  }

  test("Factorial with alternative name") {
    val factorialAlt        = LetRec("facAlt", Fun("n", If0("n", 1, Mult("n", App("facAlt", Add("n", -1))))), "facAlt")
    val translatedFactorial = translate2Fix(factorialAlt)

    assertEquals(doesNotUseRec(translatedFactorial), true)

    val factorials = List((1, 1), (2, 2), (3, 6), (4, 24), (5, 120), (6, 720))
    factorials.foreach { (n: Int, r: Int) => assertEquals(interp(App(translatedFactorial, Num(n))), Num(r)) }
  }

  test("LetRec like normal Let") {
    val program           = LetRec("x", Num(5), Add(Num(4), Id("x")))
    val translatedProgram = translate2Fix(program)
    assertEquals(doesNotUseRec(translatedProgram), true)
    assertEquals(interp(translatedProgram), interp(program))
    assertEquals(interp(translatedProgram), Num(9))
  }

  test("Shadowing") {
    val program =
      LetRec(
        "fac",
        Fun("n", If0("n", 1, Mult("n", App("fac", Add("n", +1))))), // broken, non-terminating version of factorial
        LetRec(
          "fac",
          Fun("n", If0("n", 1, Mult("n", App("fac", Add("n", -1))))), // working factorial
          App("fac", 5)
        )
      )
    val translatedProgram = translate2Fix(program)

    assertEquals(doesNotUseRec(translatedProgram), true)
    assertEquals(interp(program), Num(120))
    assertEquals(interp(translatedProgram), Num(120))
  }

  val nestingTestPrograms: Map[String, RecursiveFLAE] = Map(
    "Add LHS"  -> Add(App(factorial, 5), 1),
    "Add RHS"  -> Add(1, App(factorial, 5)),
    "Sub LHS"  -> Sub(App(factorial, 5), 1),
    "Sub RHS"  -> Sub(1, App(factorial, 5)),
    "Mult LHS" -> Mult(App(factorial, 5), 1),
    "Mult RHS" -> Mult(1, App(factorial, 5)),
    "If0 cond" -> If0(App(factorial, 5), 1, 2),
    "If0 then" -> If0(0, App(factorial, 5), 2),
    "If0 else" -> If0(1, 1, App(factorial, 5)),
    "Let"      -> Let("x", App(factorial, 5), Add("x", App(factorial, 5))),
    "App LHS"  -> App(factorial, 5),
    "App RHS"  -> App(factorial, App(factorial, 5)),
    // have to apply Fun because the resulting closures would obviously be different
    "Fun"              -> App(Fun("x", App(factorial, 5)), 1),
    "LetRec namedExpr" ->
    LetRec("fac", LetRec("fac2", Fun("n", If0("n", 1, Mult("n", App("fac2", Add("n", -1))))), App("fac2", 5)), "fac"),
    "LetRec body" ->
    LetRec(
      "fac",
      Fun("n", If0("n", 1, Mult("n", App("fac", Add("n", -1))))),
      LetRec("fac2", Fun("n", If0("n", 1, Mult("n", App("fac", Add("n", -1))))), App("fac2", 5))
    )
  )

  nestingTestPrograms.foreach { (name, program) =>
    test(s"Nesting $name") {
      val translatedProgram = translate2Fix(program)
      assertEquals(doesNotUseRec(translatedProgram), true)
      assertEquals(interp(translatedProgram), interp(program))
    }
  }
}
