package homework.assignment01basic

import BooleanInterpreter.*

class BooleanInterpreterTest extends munit.FunSuite {

  val t: Expr = True
  val f: Expr = False

  test("eval true") {assertEquals(interp(t), TrueValue)}
  test("test false") {assertEquals(interp(f), FalseValue)}

  test("negation of false") {assertEquals(interp(Not(f)), TrueValue)}
  test("negation of true") {assertEquals(interp(Not(t)), FalseValue)}

  test("conjunction TT") {assertEquals(interp(And(t, t)), TrueValue)}
  test("conjunction TF") {assertEquals(interp(And(t, f)), FalseValue)}
  test("conjunction FT") {assertEquals(interp(And(f, t)), FalseValue)}
  test("conjunction FF") {assertEquals(interp(And(f, f)), FalseValue)}

  test("disjunction TT") {assertEquals(interp(Or(t, t)), TrueValue)}
  test("disjunction TF") {assertEquals(interp(Or(t, f)), TrueValue)}
  test("disjunction FT") {assertEquals(interp(Or(f, t)), TrueValue)}
  test("disjunction FF") {assertEquals(interp(Or(f, f)), FalseValue)}

  test("double negation") {assertEquals(interp(Not(Not(t))), TrueValue)}
  test("recursive not") {assertEquals(interp(Not(And(t, t))), FalseValue)}
  test("nested conjunction") {assertEquals(interp(And(Not(f), Not(f))), TrueValue)}
  test("nested disjunction") {assertEquals(interp(Or(Not(t), Not(t))), FalseValue)}

  test("CNF") {assertEquals(interp(And(Or(Not(f), f), Or(f, t))), TrueValue)}
}
