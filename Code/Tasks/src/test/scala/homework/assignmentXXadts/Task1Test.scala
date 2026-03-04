package homework.assignmentXXadts

import homework.assignmentXXadts.Task1.*
import modularized.environmental.FullInterpreter.*
import munit.FunSuite

class Task1Test extends FunSuite {

  test("1") {
    assertEquals(
      {
        interp(App(
          booleanInterp,
          Data(
            "And",
            List(
              Data("True"),
              Data(
                "Or",
                List(
                  Data("False"),
                  Data("Not", List(Data("True")))
                )
              )
            )
          )
        ))
      },
      Constructor("False", List())
    )
  }

  test("2") {
    assertEquals(
      {
        interp(App(
          booleanInterp,
          Data(
            "Or",
            List(
              Data(
                "And",
                List(
                  Data("Not", List(Data("False"))),
                  Data("True")
                )
              ),
              Data("False")
            )
          )
        ))
      },
      Constructor("True", List())
    )
  }

  test("3") {
    assertEquals(
      {
        interp(App(
          booleanInterp,
          Data(
            "Not",
            List(Data(
              "Or",
              List(
                Data(
                  "And",
                  List(
                    Data("Not", List(Data("False"))),
                    Data("True")
                  )
                ),
                Data("False")
              )
            ))
          )
        ))
      },
      Constructor("False", List())
    )
  }

}
