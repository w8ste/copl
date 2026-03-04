package homework.assignment01basic

import homework.assignment01basic.Preprocessor.*
import munit.FunSuite

class PreprocessorTest extends FunSuite {

  def imply(lhs: Expr, rhs: Expr): Or    = Or(Not(lhs), rhs)
  def biImply(lhs: Expr, rhs: Expr): And = And(Or(Not(lhs), rhs), Or(Not(rhs), lhs))

  val t: Expr = True
  val f: Expr = False

  test("imp") { assertEquals(preproc(Imp(t, f)), imply(t, f)) }
  test("nested imp lhs") { assertEquals(preproc(Imp(Imp(f, t), t)), Or(Not(imply(f, t)), t)) }
  test("nested imp rhs") { assertEquals(preproc(Imp(t, Imp(t, t))), Or(Not(t), imply(t, t))) }
  test("bi imp") { assertEquals(preproc(BiImp(t, f)), biImply(t, f)) }
  test("nested bi imp lhs") { assertEquals(preproc(BiImp(t, Imp(f, t))), biImply(t, imply(f, t))) }
  test("nested bi imp rhs") { assertEquals(preproc(BiImp(Imp(f, t), f)), biImply(imply(f, t), f)) }
  test("nested in not") { assertEquals(preproc(Not(Imp(t, t))), Not(imply(t, t))) }
  test("nested in and lhs") { assertEquals(preproc(And(Imp(f, t), t)), And(imply(f, t), t)) }
  test("nested in and rhs") { assertEquals(preproc(And(t, Imp(f, t))), And(t, imply(f, t))) }
  test("nested in or lhs") { assertEquals(preproc(Or(Imp(f, t), t)), Or(imply(f, t), t)) }
  test("nested in or rhs") { assertEquals(preproc(Or(t, Imp(f, t))), Or(t, imply(f, t))) }
  test("true") { assertEquals(preproc(t), t) }
  test("false") { assertEquals(preproc(f), f) }

}
