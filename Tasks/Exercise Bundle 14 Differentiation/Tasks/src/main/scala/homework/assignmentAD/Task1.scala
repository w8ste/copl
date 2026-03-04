package homework.assignmentAD

object Task1 {
  trait Number[X]:
      def const(c: Double): X
      def add(a: X, b: X): X
      def mul(a: X, b: X): X
      def div(a: X, b: X): X
      def pow(a: X, n: Int): X
      def sin(a: X): X
      def cos(a: X): X
      def neg(a: X): X
      def sub(a: X, b: X): X
      def exp(a: X): X
      def log(a: X): X
      def sqrt(a: X): X

  object Number:
      extension [X: Number](a: X)
          inline def +(b: X): X     = summon[Number[X]].add(a, b)
          inline def *(b: X): X     = summon[Number[X]].mul(a, b)
          inline def /(b: X): X     = summon[Number[X]].div(a, b)
          inline def pow(n: Int): X = summon[Number[X]].pow(a, n)
          inline def sin: X         = summon[Number[X]].sin(a)
          inline def cos: X         = summon[Number[X]].cos(a)

      def const[X: Number](c: Double): X =
        summon[Number[X]].const(c)

  given Number[Double] with
      def const(c: Double): Double          = c
      def add(a: Double, b: Double): Double = a + b
      def mul(a: Double, b: Double): Double = a * b
      def div(a: Double, b: Double): Double = a / b
      def pow(a: Double, n: Int): Double    = math.pow(a, n.toDouble)
      def sin(a: Double): Double            = math.sin(a)
      def cos(a: Double): Double            = math.cos(a)
      def neg(a: Double): Double            = -a
      def sub(a: Double, b: Double): Double = a - b
      def exp(a: Double): Double            = math.exp(a)
      def log(a: Double): Double            = math.log(a)
      def sqrt(a: Double): Double           = math.sqrt(a)
      def tan(a: Double): Double            = math.tan(a)

  case class Dual(x: Double, dx: Double)

  given Number[Dual] with
      def const(c: Double): Dual = ???

      def add(a: Dual, b: Dual): Dual = ???

      def mul(a: Dual, b: Dual): Dual = ???

      def div(a: Dual, b: Dual): Dual = ???

      def pow(a: Dual, n: Int): Dual = ???

      def sin(a: Dual): Dual = ???

      def cos(a: Dual): Dual = ???

      def neg(a: Dual): Dual = ???

      def sub(a: Dual, b: Dual): Dual = ???

      def exp(a: Dual): Dual = ???

      def log(a: Dual): Dual = ???

      def sqrt(a: Dual): Dual = ???

      def tan(a: Dual): Dual = ???

  import Number.*

  // example:
  // a generic error function, that can be called either to return a Double or a Dual

  def genericError[N: Number](x: N): N =
    const[N](2.0) * x + x * x

}
