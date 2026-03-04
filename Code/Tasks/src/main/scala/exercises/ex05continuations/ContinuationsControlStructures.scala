package exercises.ex05continuations

import interpreters.continuations.Continuations.*

object ContinuationsControlStructures {

  import scala.language.implicitConversions

  implicit def symbolToKCFLAE(symbol: String): Id = Id(symbol)
  implicit def intToKCFLAE(n: Int): Num           = Num(n)

  val pair: Fun   = Fun("l", Fun("r", Fun("pair", App(App("pair", "l"), "r"))))
  def first: Fun  = Fun("p", App("p", Fun("l", Fun("r", "l"))))
  def second: Fun = Fun("p", App("p", Fun("l", Fun("r", "r"))))

  def fst(p: KCFLAE): App                 = App(first, p)
  def snd(p: KCFLAE): App                 = App(second, p)
  def makePair(l: KCFLAE, r: KCFLAE): App = App(App(pair, l), r)

  // our version of `try/catch`

  def attempt(expr: KCFLAE, orElse: KCFLAE) = ???

  // our version of `throw`
  def fail(msg: KCFLAE): App = ???

  def main(args: Array[String]): Unit = {

    val input = Num(11)

    println(interp(
      attempt(
        If0(Sub(input, Num(10)), Num(42), fail(Msg("not 10!"))),
        Fun("exception", Msg("handled"))
      )
    ))

    def fac(n: Int) = {
      snd(
        doWhile(
          makePair(Num(n), Num(1)),
          Fun("(n, acc)", fst("(n, acc)")),
          Fun(
            "(n, acc)",
            makePair(
              Sub(App(first, "(n, acc)"), 1),
              Mult(App(first, "(n, acc)"), App(second, "(n, acc)"))
            )
          )
        )
      )
    }

    println(interp(Add(fac(5), fac(3))))

  }

  // our version of `do … while`
  def doWhile(start: KCFLAE, condition: KCFLAE, expr: KCFLAE): Let = ???

}
