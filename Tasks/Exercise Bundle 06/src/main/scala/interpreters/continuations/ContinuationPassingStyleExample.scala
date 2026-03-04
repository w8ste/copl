package interpreters.continuations

import scala.util.boundary

object ContinuationPassingStyleExample {

  sealed trait Nat {
    def toInt: Int = this match {
      case Zero    => 0
      case Succ(n) => n.toInt + 1
    }
  }
  case object Zero        extends Nat
  case class Succ(n: Nat) extends Nat

  sealed trait Tree
  case object Leaf                                 extends Tree
  case class Node(n: Nat, left: Tree, right: Tree) extends Tree

  def add(n: Nat, m: Nat): Nat = n match {
    case Zero       => m
    case Succ(pred) => Succ(add(pred, m))
  }

  def sum(t: Tree): Nat = t match {
    case Leaf                 => Zero
    case Node(n, left, right) =>
      val lv      = sum(left)
      val rv      = sum(right)
      val tempNat = add(lv, n)
      add(tempNat, rv)
  }

  def addK(n: Nat, m: Nat, k: Nat => Nothing): Nothing = n match {
    case Zero       => k(m)
    case Succ(pred) => addK(pred, m, res => k(Succ(res)))
  }

  def sumK(t: Tree, k: Nat => Nothing): Nothing = t match {
    case Leaf                 => k(Zero)
    case Node(n, left, right) =>
      sumK(
        left,
        lv =>
          sumK(
            right,
            rv =>
              addK(
                lv,
                n,
                tempNat =>
                  addK(tempNat, rv, k)
              )
          )
      )
  }

  def main(args: Array[String]): Unit = {

    val nat3 = Succ(Succ(Succ(Zero)))
    val nat4 = Succ(nat3)
    val nat5 = Succ(nat4)

    val t = Node(nat3, Node(nat5, Leaf, Leaf), Node(nat4, Leaf, Leaf))

    println(sum(t).toInt)

    // boundar/break is a little scala helper library that emulates `break` statements known from while loops and similar.
    // it is based on exceptions and works quite nicely for our examples to
    // transform from the continuation passing world back into the normal world
    val res = boundary {
      sumK(t, (n: Nat) => boundary.break(n))
    }
    println(res.toInt)
  }

}
