package homework.assignment03functions.part2

import homework.assignment03functions.part2.definitions.MFAEBase.*

import scala.language.implicitConversions

object HigherOrderMFAE {

  // Some implicit conversions you may find helpful
  implicit def symbolToList(x: String): List[String]  = List(x)
  implicit def faeToList(x: MFAE): List[MFAE]         = List(x)
  implicit def symbolToFAE(x: String): MId            = MId(x)
  implicit def intToFAE(x: Int): MNum                 = MNum(x)
  implicit def symbolToFAEList(x: String): List[MFAE] = faeToList(symbolToFAE(x))

  // example
  val exampleFun: MFun = MFun(List("a", "b"), MAdd("a", "b"))

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  ???

}
