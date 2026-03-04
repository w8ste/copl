package exercises.ex11typeclasses

import modularized.stateful.Sequential

/** IOMonad simply extends the Monad interface for IO.
  * The implementation of the functions simply wrap the arguments in a data structure.
  * In other words, what the IOMonad is doing is simply to record the things that should be done.
  */
object IOMonad extends Sequential[IO] {
  def ret[A](v: A): IO[A] = IO.Pure(v)
  extension [A](m: IO[A])
      def andThen[B](f: A => IO[B]): IO[B] =
        IO.AndThen(m, f)
}

// We declare that he IOMonad object is the “given” instance when we implicitly look for a value of type `Monad[IO]`.
// There is some shorthand syntax to directly define the object above as “given”, but we are explicit here.
// Shorthand looks like:
// given Monad[IO] with { def ret .... }
given Sequential[IO] = IOMonad

// IO is just a sealed trait, similar to the interpreter expressions.
// In fact, we will soon write an interpreter to convert the IO datastructure into actual side effects.
sealed trait IO[V]

object IO {

  // We explicitly model some effects, specifically reading and printing strings.
  case object ReadLine              extends IO[String]
  case class PrintLine(str: String) extends IO[Unit]

  // These are required to implement the Monad typeclass. They record which monad functions were used and what their parameters are.
  case class Pure[A](v: A)                                   extends IO[A]
  case class AndThen[A, B](first: IO[A], second: A => IO[B]) extends IO[B]

  // We don’t have to explicitly add a constructor for any effect we want to execute,
  // and can instead just “capture” any Scala function with side effects for later use.
  // However, doing so means that the interpreter below does not know what effect it will execute,
  // so the interpreter can not be specialized for different effects.
  case class AnyScalaEffect[A](f: () => A) extends IO[A]

  // These are just convenience functions. These specifically are not very exciting
  def readline: IO[String]             = ReadLine
  def printline(str: String): IO[Unit] = PrintLine(str)
}

// using IO, we can define programs as datastructures
// this is what you do in haskell, when you define `main :: IO ()`
val program =
    import exercises.ex11typeclasses.IO.{printline, readline}
    import IOMonad.ret
    printline("please enter your name").andThen: _ =>
        readline.andThen: name =>
            printline(s"Hello $name").andThen: _ =>
                ret(name)

// Scala has special syntax for objects with `flatMap` (i.e., `andThen`) and `map` (a combination of andThen and Pure) methods.
// The following is the same as above, just using `for` syntax instead.
val programUsingForSyntax =
    import exercises.ex11typeclasses.IO.{printline, readline}
    for
        _    <- printline("please enter your name")
        name <- readline
        _    <- printline(s"Hello $name")
    yield name

// When we actually want to execute the effects described by the IO datastructure, we do what we always do:
// We pass the datastructure to an interpreter that takes our constructors and converts them into actual effects (and a value).
def interpret[V](io: IO[V]): V =
  io match
      case IO.AndThen(first, second) =>
        val v = interpret(first)
        interpret(second(v))
      case IO.PrintLine(str)    => println(str)
      case IO.Pure(v)           => v
      case IO.ReadLine          => scala.io.StdIn.readLine()
      case IO.AnyScalaEffect(f) => f()

@main
def run() =
    println(program)
    interpret(program)
