package exercises.ex11typeclasses

import modularized.stateful.Sequential

/** This is an alias which allows us to define a monad instance for just a plain type A */
type Identitiy[A] = A

/** The identity monad just returns values and applies a function without carrying around any additional context.
  * We use this to demonstrate that the interpreter below can be used without any specific context (at least for the parts that do not require state
  */
given Sequential[Identitiy] with {
  override def ret[V](v: V): Identitiy[V] = v
  extension [A](m: Identitiy[A])
      override def andThen[B](f: A => Identitiy[B]): Identitiy[B] = f(m)
}
