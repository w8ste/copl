package modularized.stateful

/** We model sequential execution in our stateful interpreters.
  * You can think of each interpreter of returning a pipeline of type `M[A]`,
  * where `M` describes the (sequential) processing in this pipeline,
  * while `A` is the result of the pipeline.
  *
  * This typeclass allows us to do two things with any type of pipeline:
  * • we can add one more step that returns a new value of type B
  * • we can return a value without doing any of the pipeline specific processing.
  *
  * This typeclass is often called Monad – a term from category theory that reflects
  * that this structure is much more generic than what we use it for here.
  */
trait Sequential[M[_]] {

  // extension allows us to syntactically call this function as if it was defined on m.
  // that is, we can write it as `m.andThen(f)` instead of `andThen(m)(f)`
  extension [A](steps: M[A]) def andThen[B](nextStep: A => M[B]): M[B]

  def ret[A](a: A): M[A]

  // these two definitions would allow us to use Scala’s `for` syntax with our pipeline
  extension [A](steps: M[A]) final def flatMap[B](nextStep: A => M[B]): M[B] = steps.andThen(nextStep)
  extension [A](steps: M[A]) final def map[B](nextStep: A => B): M[B]        = steps.andThen(v => ret(nextStep(v)))
}
object Sequential {
  def ret[A, S[_]](a: A)(using seq: Sequential[S]): S[A] = seq.ret(a)

  def sequence[A, S[_]](list: List[S[A]])(using seq: Sequential[S]): S[List[A]] =
    list match
        case Nil    => Sequential.ret(Nil)
        case h :: t => h.andThen(res => sequence(t).map(inner => res :: inner))
}

/** To model mutable state in our language, we will have a pipeline consisting of these Step values.
  * Each step represents a transformation that takes a value of some state S, and produces an output of type A, and a new state.
  *
  * This is more commonly called State or StateTransformer, but [[Step]] is closer to the actual usage,
  * where each instance of the datatype represents one step in a sequence of transformations to some state.
  * Though, as we see below (in [[stepSequential]]), multiple steps are often composed into one larger step.
  */
case class Step[S, +A](run: S => (A, S))

/** Instance of Pipeline for State.
  * Note that the =>> is a “type lambda”. Monad requires a type constructor with a single parameter (M[_] only has one argument), but state has two type parameters.
  * So we use this type level function to specify which of the two parameters is the one that Monad talks about (it’s the A) while the other is part of the definition of this given.
  * In Haskell we did not need to do anything specific for state, as type parameters are also curried.
  */
given stepSequential[S]: Sequential[[A] =>> Step[S, A]] with {
  override def ret[A](v: A): Step[S, A] = Step(s => (v, s))
  extension [A](m: Step[S, A])
      override def andThen[B](nextStep: A => Step[S, B]): Step[S, B] =
        Step { s =>
          val (a, nextA) = m.run(s)
          val nextB      = nextStep(a)
          nextB.run(nextA)
        }
}

// reason about locations abstractly, though really they are just integers
type Location = Int

/** The StoreAPI abstracts over the concrete type of store we have.
  * This is similar to how monad abstracts over sequential composition, but much more specialized to our concrete use case.
  * Note that this type class always just produces values of type S, but never takes them as input.
  * This is because it essentially models “mutable state” which reads/writes the current state from the context.
  * This context will be provided by the State type below.
  */
trait StoreApi[V, Pipe[_]] {
  def malloc(value: V, roots: Set[Location]): Pipe[Location]
  def update(location: Location, value: V): Pipe[Unit]
  def lookup(location: Location): Pipe[V]
}

/** This API abstracts over the combination of environments and stores,
  * for those cases that do not explicitly need to reason about stores at all,
  * but still have to interact with the store because values form the environment now belong in the store.
  */
trait EnvironmentStoreApi[V, Env, Pipe[_]] {
  extension (env: Env)
      def bind(name: String, value: V): Pipe[Env]
      def lookup(name: String): Pipe[V]
      def update(name: String, value: V): Pipe[Env]
      def stack(other: Env): Env
      def reachable(value: V): Env
}

/** An extension on environments to also track the set of root locations required for garbage collection. */
trait HasRoots[E] {
  extension (e: E)
      def roots: Set[Location]
}
