package lectures.modularization

import scala.util.NotGiven

/** Scala does have nested packages, and allows multiple packages per file. */
package Problem {

  /** Here is our very basic interpreter for Arithmetic expressions. */
  object Arithmetic {
    sealed trait Expr
    case class Num(n: Int)               extends Expr
    case class Add(lhs: Expr, rhs: Expr) extends Expr
    case class Sub(lhs: Expr, rhs: Expr) extends Expr

    type Value = Int

    def interp(expr: Expr): Value = expr match
        case Num(n)        => n
        case Add(lhs, rhs) => interp(lhs) + interp(rhs)
        case Sub(lhs, rhs) => interp(lhs) - interp(rhs)
  }

  /** And here is our interpreter for the basic lambda calculus using Environments and static scoping. */
  object Lambda {
    sealed trait Expr
    case class Id(name: String)               extends Expr
    case class Fun(param: String, body: Expr) extends Expr
    case class App(funExp: Expr, arg: Expr)   extends Expr

    case class Closure(param: String, body: Expr, env: Env)

    type Value = Closure
    type Env   = Map[String, Value]

    def interp(expr: Expr, env: Env): Value = expr match {

      case Id(name) => env.getOrElse(name, sys.error(s"unbound variable $name"))

      case Fun(param, body) => Closure(param, body, env)

      case App(funExpr, argExpr) =>
        interp(funExpr, env) match {
          case Closure(param, body, funEnv) =>
            val argVal      = interp(argExpr, env)
            val extendedEnv = funEnv + (param -> argVal)
            interp(body, extendedEnv)
        }

    }
  }

  /** can we combine them … ? */
  object LambdaArithmetic {

    /** Union type to the rescue!
      * A union combines value of both types into a new type, so our expressions could be both lambda expressions or
      */
    type Expr  = Lambda.Expr | Arithmetic.Expr
    type Value = Lambda.Value | Arithmetic.Value

    def interp(expr: Expr, env: Lambda.Env = Map.empty): Value = expr match {
      // Handle Lambda expressions
      case lambdaExpr: Lambda.Expr => Lambda.interp(lambdaExpr, env)

      // Handle Arithmetic expressions
      case arithmeticExpr: Arithmetic.Expr => Arithmetic.interp(arithmeticExpr)
    }

    /** great, let’s test this! */
    def main(args: Array[String]): Unit = {

      /* does not compile … Lambda.Expr may only contain Lambda.Expr, and Arithmetic.Expr may only contain Arithmetic.Expr.
       * But we need them to contain the combined Expr in this object, we somehow need to abstract over the expression type. */
      //    val expr = App(
      //      Fun(
      //        "x",
      //        Add(Id("x"), Num(2))
      //      ),
      //      Num(2)
      //    )

    }

  }
}

package DependentTypes {

  /** In Scala, traits can have abstract type members like this */
  trait AbstractInterpreter {
    type Expr
    type Value

    /** We will see later how to generalize over the environment type */
    type Env

    def interp(expr: Expr, env: Env): Value
  }

  /** we can create multiple objects that inherit from the above trait, and each of them */
  object AE1 extends AbstractInterpreter {
    override def interp(expr: Expr, env: Env): Value = ???
  }
  object AE2 extends AbstractInterpreter {
    override def interp(expr: Expr, env: Env): Value = ???
  }

  /** Because the two Expr are abstract, they are different because they are defined on different objects. */
  val test = summon[NotGiven[AE1.Expr =:= AE2.Expr]]

  /*
   * Because the type *depends* on a concrete value (AE1 and AE2 are concrete objects, which are values in Scala) these are called *dependent types*.
   * Though, Scala only supports a somewhat limit form of dependent typing, however having abstract types and subtyping is enough for us.
   */

  /** Note Arithmetic is a trait, not an object */
  trait AbstractArithmetic extends AbstractInterpreter {

    /* We cannot extend an abstract type, it is only possible to extend traits and classes
     * (the abstract type could later be defined as something that is not extendable) */
    // case class Num(n: Int) extends Expr

    /** However, we can use the abstract type as the type of the inner expressions for Add and Sub */
    sealed trait ArithmeticExpr
    case class Num(n: Int)               extends ArithmeticExpr
    case class Add(lhs: Expr, rhs: Expr) extends ArithmeticExpr
    case class Sub(lhs: Expr, rhs: Expr) extends ArithmeticExpr

    /** We have an interpreter function for just ArithmeticExpr */
    def interpArithmetic(expr: ArithmeticExpr, env: Env): Num = expr match
        case Num(n)        => Num(n)
        case Add(lhs, rhs) => interpBinop(lhs, rhs, _ + _, env)
        case Sub(lhs, rhs) => interpBinop(lhs, rhs, _ - _, env)

    /** Note that lhs and rhs are abstract expressions, which we interpret to an abstract Value result */
    private def interpBinop(lhs: Expr, rhs: Expr, op: (Int, Int) => Int, env: Env): Num =
        val leftValue: Value  = interp(lhs, env)
        val rightValue: Value = interp(rhs, env)
        (leftValue, rightValue) match
            case (Num(n1), Num(n2)) => Num(op(n1, n2))
            case v                  => sys.error(s"Expected numeric values but got $v")
  }

  /** Making things concrete */
  object Arithmetic extends AbstractArithmetic {
    type Expr  = ArithmeticExpr
    type Value = Num

    type Env = Map[String, Value]

    override def interp(expr: Expr, env: Env): Value = interpArithmetic(expr, env)

    // simple test
    def main(args: Array[String]): Unit = {
      val expr  = Add(Num(1), Num(2))
      val value = interp(expr, Map.empty)
      println(value)
    }
  }

  trait AbstractLambda extends AbstractInterpreter {
    sealed trait LambdaExpr
    case class Id(name: String)               extends LambdaExpr
    case class Fun(param: String, body: Expr) extends LambdaExpr
    case class App(funExp: Expr, arg: Expr)   extends LambdaExpr

    case class Closure(param: String, body: Expr, env: Env)

    /** We will see how to keep the environment generic later. */
    type Env = Map[String, Value]

    /** Note that we return a Value or a Closure */
    def interpLambda(expr: LambdaExpr, env: Env): Value | Closure = expr match {

      case Id(name) => env.getOrElse(name, sys.error(s"unbound variable $name"))

      case Fun(param, body) => Closure(param, body, env)

      case App(funExpr, argExpr) =>
        interp(funExpr, env) match {
          case Closure(param, body, funEnv) =>
            val argVal      = interp(argExpr, env)
            val extendedEnv = funEnv + (param -> argVal)
            interp(body, extendedEnv)
        }

    }
  }

  /** We can now realize our original goal of just combining the two definitions.
    * Though we do need to write some boilerplate (defining the abstract values) this is straightforward,
    * and importantly it’s hard to have subtle bugs&variations in this part of the code (as opposed to copy&pasting)
    */
  object LambdaArithmetic extends AbstractLambda with AbstractArithmetic {

    /** Expr is defined to be a *union type* of all all values of LambdaExpr and all Values of ArithmeticExpr.
      * You can read this as Expr is either a LambdaExpr or a ArithmeticExpr (which is why it uses the | symbol)
      */
    type Expr  = LambdaExpr | ArithmeticExpr
    type Value = Closure | Num

    /** The full interp function must check what type expr is and delegate accordingly.
      * There are ways to reduce boilerplate here,
      * but this version is straightforward and checks that the pattern match is exhaustive
      */
    override def interp(expr: Expr, env: Env): Value = expr match {
      case lambdaExpr: LambdaExpr         => interpLambda(lambdaExpr, env)
      case arithmeticExpr: ArithmeticExpr => interpArithmetic(arithmeticExpr, env)
    }

    // Running the test in main
    def main(args: Array[String]): Unit = {

      // Define a combined Lambda and Arithmetic expression:
      val expr = App(
        Fun(
          "x",
          Add(Id("x"), Num(3)) // Add arithmetic inside the lambda body
        ),
        Num(7) // Argument to the lambda
      )

      // Interpreting the expression
      val result = interp(expr, Map.empty)

      // Print the result
      result match {
        case Num(value) => println(s"Result: $value") // Expecting 7 + 3 = 10
        case _          => println("Unexpected result")
      }
    }
  }

  /** See the [[modularized]] package for a complete implementation of everything we have seen using this pattern.
    * Notably see [[modularized.Expressions]] for the abstract AST definitions, and [[modularized.environmental.FullInterpreter]] for an interpreter that combines all features we have seen (and some we have not yet seen)
    */
}

/** In the abstract interpreter above, we had the Env type be concrete (a `Map[String, Value]`).
  * However, remember we also used `String => Value` to represent environments, and the `LetRec` construct needed some
  * way to bind values in the environment late (either through mutation or lazy evaluation).
  * How can we make our environment type generic?
  */
package Typeclasses {

  import lectures.modularization.DependentTypes.AbstractInterpreter

  /** copy paste one more time … */
  trait AbstractEnvLambdaUsingFunctions extends AbstractInterpreter {
    sealed trait LambdaExpr
    case class Id(name: String)               extends LambdaExpr
    case class Fun(param: String, body: Expr) extends LambdaExpr
    case class App(funExp: Expr, arg: Expr)   extends LambdaExpr

    case class Closure(param: String, body: Expr, env: Env)

    /** Note, Env remains generic, but instead we have these functions to interact with env.
      * We define them as extension defs for some nice syntactic sugar.
      */
    extension (env: Env) def lookup(name: String): Option[Value]
    extension (env: Env) def updated(name: String, value: Value): Env

    /** same as before, but using the functions above, instead of directly operating on maps */
    def interpLambda(expr: LambdaExpr, env: Env): Value | Closure = expr match {

      case Id(name) => env.lookup(name).getOrElse(sys.error(s"unbound variable $name"))

      case Fun(param, body) => Closure(param, body, env)

      case App(funExpr, argExpr) =>
        interp(funExpr, env) match {
          case Closure(param, body, funEnv) =>
            val argVal      = interp(argExpr, env)
            val extendedEnv = funEnv.updated(param, argVal)
            interp(body, extendedEnv)
        }

    }
  }

  /** Its design wise strange to have the lookup and updated functions as part of the abstract interpreter above.
    * Let’s instead extract them into their own trait.
    * Such a pattern, where a bunch of related functions on a generic type are extracted into a trait is called a Typeclass.
    */
  trait Environment[Env, T] {
    extension (env: Env)
        def lookup(name: String): Option[T]
        def updated(name: String, value: T): Env
  }

  trait AbstractExample {
    type Value
    type Env

    /** define an abstract value of the typeclass */
    val envSyntax: Environment[Env, Value]

    /** import the functions so we can use them even though they are abstract */
    import envSyntax.{lookup, updated}

    def test(env: Env): Env = env.updated("update", env.lookup("defined").get)
  }

  object ConcreteExample extends AbstractExample {

    type Value = Int
    type Env   = Map[String, Value]

    /** we provide a concrete *instance* of the type class – just an implementation of the trait. */
    override val envSyntax: Environment[Env, Value] = new Environment[Env, Int] {
      extension (env: Env)
          override def lookup(name: String): Option[Value]      = env.get(name)
          override def updated(name: String, value: Value): Env = env.updated(name, value)
    }

    def main(args: Array[String]): Unit =
      println(test(Map("defined" -> 10)))

  }

  /** Scala has some special syntax that help working with typeclasses, or rather,
    * that help working with specific *given* instances that are very type directed.
    * For example, in the above, we look for an implementation of `Environment[Env, Value]`
    */
  object Syntax {
    type Env
    type Value

    /** notably, the `using` syntax allows us to specify that the `test` method needs some instance of the specified type.
      * However, you generally do not have to specify this parameter when you call the function,
      * instead the compiler will automatically find a `given` instance (see below) of a matching type.
      *
      * Extension methods defined on such a `given` value are automatically imported specifically to enable the typeclass pattern.
      */
    def test(env: Env)(using Environment[Env, Value]): Env = env.updated("update", env.lookup("defined").get)

    given Environment[Env, Value] with {
      extension (env: Env)
          override def lookup(name: String): Option[Value]      = ???
          override def updated(name: String, value: Value): Env = ???
    }

  }

}

/** The above abstract over the basic interpreters.
  * However, when we introduced state we needed to introduce a store to track the changing values, see [[interpreters.memory.GarbageCollection]],
  * and when we introduced continuations, we needed to rewrite our interpreters in continuation passing styile, see [[interpreters.continuations.Continuations]]
  *
  * Can we abstract over these differences?
  */
package AbstractResults {

  /** The commonality of both state and continuations is that in addition to computing a value,
    * a “context” of the interpretation is changed whenever a call to `interpret` is made
    * The “context” is the current store, or the current continuation in the respective interpreters.
    */
  trait AbstractSequentialAdd {

    type Value
    type Expr

    type Context[_]

    sealed trait SequentialExpr
    case class Num(n: Int)               extends SequentialExpr
    case class Add(lhs: Expr, rhs: Expr) extends SequentialExpr

    /** state that context allows sequential composition (see below), but that finding an instance is deferred to later */
    given Sequential[Context] = scala.compiletime.deferred

    def interp(
        expr: Expr,
    ): Context[Num] = expr match
        case Num(n)        => Sequential.ret(Num(n))
        case Add(lhs, rhs) =>
          interp(lhs).andThen { case Num(lv) =>
            interp(rhs).andThen { case Num(rv) =>
              Sequential.ret(Num(lv + rv))
            }
          }
          /* or use syntactic sugar: */
//        for {
//          Num(lv) <- interp(lhs)
//          Num(rv) <- interp(rhs)
//        } yield Num(lv + rv)

  }

  trait Sequential[M[_]] {

    extension [A](steps: M[A]) def andThen[B](nextStep: A => M[B]): M[B]

    def ret[A](a: A): M[A]

    // these two definitions would allow us to use Scala’s `for` syntax with our pipeline
    extension [A](steps: M[A]) final def flatMap[B](nextStep: A => M[B]): M[B] = steps.andThen(nextStep)
    extension [A](steps: M[A]) final def map[B](nextStep: A => B): M[B]        = steps.andThen(v => ret(nextStep(v)))
  }
  object Sequential {
    def ret[A, S[_]](a: A)(using seq: Sequential[S]): S[A] = seq.ret(a)
  }

  object SimpleSequentialExample extends AbstractSequentialAdd {
    type Value      = Num
    type Expr       = SequentialExpr
    type Context[A] = Option[A]

    override given Sequential[Context] with {
      extension [A](steps: Option[A])
          override def andThen[B](nextStep: A => Option[B]): Option[B] = steps.flatMap(nextStep)
      override def ret[A](a: A): Option[A]                             = Some(a)
    }

    def main(args: Array[String]): Unit = {

      // Example test for the SimpleSequentialExample interpreter
      val expr = Add(Num(10), Num(20))

      val result = SimpleSequentialExample.interp(expr)

      result match {
        case Some(Num(value)) =>
          println(s"Test Passed: Result is $value")
        case _ =>
          println("Test Failed: Unexpected result")
      }
    }
  }

}
