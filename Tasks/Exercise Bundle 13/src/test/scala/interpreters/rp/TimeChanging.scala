package interpreters.rp

trait GuessingGame extends TimeChanging {
  // tests if the user guessed
  def guessingGame(input: Value): Let =
    Let(
      "secret",
      Num(42),
      Let(
        "input",
        Source(input),
        Let(
          "difference",
          Sub(Id("input"), Id("secret")),
          Let(
            "signum",
            Signum(Id("difference")),
            Id("signum"),
          )
        )
      )
    )
}

object IOExample {

  object UnitTime extends GuessingGame {
    type Time = Unit
  }

  def main(args: Array[String]): Unit = {
    import UnitTime.*

    var userGuess = -1

    val runningProgram = interp(guessingGame(time => Num(userGuess)), Map.empty)

    println("guess a number")
    while
        scala.io.StdIn.readLine().strip().nn.toIntOption match
            case None =>
              println("invalid guess!")
              false
            case Some(n) =>
              userGuess = n
              runningProgram(()) match
                  case Num(0) =>
                    println("you guessed correctly")
                    false
                  case Num(-1) =>
                    println("too small!")
                    true
                  case Num(1) =>
                    println("too large!")
                    true
                  case other => sys.error("program misbehaved")
    do ()

  }
}

class TimeChangingTests extends munit.FunSuite {

  object IntTime extends GuessingGame {
    override type Time = Int
  }

  test("int time") {
    import IntTime.*

    val userInput = List(1, 99, 50, 42)

    val runningProgram = interp(guessingGame(time => Num(userInput(time))), Map.empty)

    assertEquals(runningProgram(0), Num(-1))
    assertEquals(runningProgram(1), Num(1))
    assertEquals(runningProgram(2), Num(1))
    assertEquals(runningProgram(3), Num(0))

  }

}
