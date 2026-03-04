package exercises.ex01

/*
  Task 1: Interpreter for Arithmetic expressions

  In this task, we will implement an interpreter that evaluates arithmetic expressions (+, *)
  to Peano Numbers.
 */
object ArithmeticExpressions {

  // Definition of natural numbers as peano numbers
  sealed trait PeanoNumber {
    def toInt: Int = this match {
      case Zero    => 0
      case Succ(p) => p.toInt + 1
    }

    // Define + operator on Peano numbers
    def +(other: PeanoNumber): PeanoNumber =
      add(this, other)

    // Define * operator on peano numbers
    def *(other: PeanoNumber): PeanoNumber =
      mul(this, other)
  }

  case object Zero                   extends PeanoNumber
  case class Succ(pred: PeanoNumber) extends PeanoNumber

  // Addition of Peano numbers (Solution of last homework)
  def add(lhs: PeanoNumber, rhs: PeanoNumber): PeanoNumber = lhs match {
    case Zero    => rhs
    case Succ(n) => add(n, Succ(rhs))
  }

  /*
   * 1) Implement multiplication on Peano Numbers.
   */
  def mul(lhs: PeanoNumber, rhs: PeanoNumber): PeanoNumber =
    rhs match {
      case Zero => Zero
      case Succ(b) => add(lhs, mul(lhs, b))
    } 

  /*
   * 2) Extend the Arithmetic expressions AE with multiplication. Then implement an interpreter for AE.
   */
  trait AE
  case class Num(n: PeanoNumber)   extends AE
  case class Add(lhs: AE, rhs: AE) extends AE
  case class Mult(lhs: AE, rhs: AE) extends AE  
  // add multiplication

  def interp(ae: AE): PeanoNumber =
    ae match {
      case Num(n) => n
      case Add(lhs, rhs) => add(interp(lhs), interp(rhs))
      case Mult(lhs, rhs) => mul(interp(lhs), interp(rhs))
    }


  /*
   * 3) We extend the Arithmetic expression with Inc which increases the value of an arithmetic
   * 	  expression by 1. Write a preprocessor that transforms an arithmetic expression with Inc to
   *	  an expression that does not use Inc.
   */
  case class Inc(e: AE) extends AE

  def preprocess(ae: AE): AE = 
    ae match {
      case Inc(e) => Add(e, Num(Succ(Zero)))
      case _ => ae
    }


  def main(args: Array[String]): Unit = {
    val One   = Succ(Zero)
    val Two   = Succ(One)
    val Three = Succ(Two)

    println((Two * Three).toInt)

    val e1 = Mult(Num(Two), Add(Num(Three), Num(One)))
    println(interp(e1).toInt)

    val e2 = Inc(Mult(Num(Two), Num(Three)))
    println(interp(preprocess(e2)).toInt)
  }

}
