package homework.assignment00hello

object PeanoNumbers {
  sealed trait PeanoNumber
  case object Zero                   extends PeanoNumber
  case class Succ(pred: PeanoNumber) extends PeanoNumber

  // pattern matching example
  def pred(x: PeanoNumber): PeanoNumber = x match {
    case Zero    => Zero
    case Succ(p) => p
  }
  def succ(x: PeanoNumber): PeanoNumber = Succ(x)

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  // your solution here
  def add(n: PeanoNumber, m: PeanoNumber): PeanoNumber = ???

}
