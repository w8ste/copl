package homework.assignment03functions.part2

import homework.assignment03functions.part2.HigherOrderMFAE.*
import homework.assignment03functions.part2.definitions.MFAEBase.*

class HigherOrderMFAETest extends munit.FunSuite {

  // example
  test("1") { assertEquals(interpMultiarg(MApp(exampleFun, List(1, 2))), MNum(3)) }
  test("2") { assertEquals(interpMultiarg(MApp(exampleFun, List(40, 2))), MNum(42)) }

  // If you are unfamiliar with implementing unit tests in Scala,
  // the test cases from previous examples can be used as examples

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  
  // TODO your functions and tests:
}
