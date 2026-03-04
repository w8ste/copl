package homework.assignmentXXadts

import homework.assignmentXXadts.Task2.*
import modularized.environmental.FullInterpreter.*
import munit.FunSuite

class Task2Test extends FunSuite {

  def testIt(testname: String, expected: Int, actual: Data): Unit = {
    test(testname) {
      assertEquals(Constructor("NumV", List(expected)), interp(App(App(flaeInterp, actual), Data("Nil", List.empty))))
    }
  }

  // Add, Mul
  val twoPlusThree: Data                      = Data("Add", List(Data("Num", List(2)), Data("Num", List(3))))
  val twoPlusThreePlusTwoPlusThree: Data      = Data("Add", List(twoPlusThree, twoPlusThree))
  val threeTimesFour: Data                    = Data("Mult", List(Data("Num", List(3)), Data("Num", List(4))))
  val threeTimesFourTimesThreeTimesFour: Data = Data("Mult", List(threeTimesFour, threeTimesFour))
  testIt("1.add", 5, twoPlusThree)
  testIt("1.add'", 10, twoPlusThreePlusTwoPlusThree)
  testIt("1.mul", 12, threeTimesFour)
  testIt("1.mul'", 144, threeTimesFourTimesThreeTimesFour)

  // Let and Sub
  val flaeLet: Data =
    Data(
      "Let",
      List(
        Sym("x"),
        Data("Num", List(5)),
        Data("Id", List(Sym("x")))
      )
    )
  testIt("2.1 let", 5, flaeLet)

  // Let and Sub
  val flaeLetSub: Data =
    Data(
      "Let",
      List(
        Sym("x"),
        Data("Num", List(5)),
        Data(
          "Let",
          List(
            Sym("y"),
            Data("Num", List(2)),
            Data("Sub", List(Data("Id", List(Sym("x"))), Data("Id", List(Sym("y")))))
          )
        )
      )
    )
  testIt("2.1 let sub", 3, flaeLetSub)

  // Let and Fun and App
  val flaeLetFunApp: Data = Data(
    "App",
    List(
      Data(
        "Let",
        List(
          Sym("x"),
          Data("Num", List(5)),
          Data("Fun", List(Sym("n"), Data("Add", List(Data("Id", List(Sym("n"))), Data("Id", List(Sym("x")))))))
        )
      ),
      Data("Num", List(7))
    )
  )
  testIt("3.1 let fun app add", 12, flaeLetFunApp)

}
