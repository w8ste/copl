package homework.assignment05continuations

object Task1 {
  // Reimplement the `sumList` function below as `sumListCPS` in continuation-
  // passing style. This implies that every call has to be a tail-call.
  def sumList(list: List[Int]): Int = list match {
    case Nil     => 0
    case x :: xs => x + sumList(xs)
  }

  def sumListCPS(list: List[Int], k: Int => Nothing): Nothing = ???

  // Reimplement the `fib` function below as `fibCPS` in continuation-passing
  // style. This implies that every call has to be a tail-call.
  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 1) + fib(n - 2)
  }

  def fibCPS(n: Int, k: Int => Nothing): Nothing = ???

  // Now compose your `sumListCPS` and `fibCPS` functions in continuation-
  // passing style.
  def fibAfterSumListCPS(list: List[Int], k: Int => Nothing): Nothing = ???

  // Finally, convert `map` to `mapCPS`. Note that `mapCPS` should take an
  // `opCPS` argument, i.e., the mapping function is itself in CPS.
  def map[A, B](op: A => B, list: List[A]): List[B] = list match {
    case Nil     => Nil
    case a :: as => op(a) :: map(op, as)
  }

  def mapCPS[A, B](
      opCPS: (A, B => Nothing) => Nothing,
      list: List[A],
      k: List[B] => Nothing
  ): Nothing = ???

}
