package interpreters.continuations.delimited

// first read ShiftResetPurely
// this file contains the exactly same implementation,
// but hides the passing of continuations Cont and metacontinuations Cont2
// behind for/yield syntactic sugar

object ShiftResetMonadic {

  sealed trait KCFLAE
  case class Num(n: Int)                                      extends KCFLAE
  case class Add(lhs: KCFLAE, rhs: KCFLAE)                    extends KCFLAE
  case class Sub(lhs: KCFLAE, rhs: KCFLAE)                    extends KCFLAE
  case class Mult(lhs: KCFLAE, rhs: KCFLAE)                   extends KCFLAE
  case class Let(name: String, namedE: KCFLAE, body: KCFLAE)  extends KCFLAE
  case class Id(name: String)                                 extends KCFLAE
  case class Fun(param: String, body: KCFLAE)                 extends KCFLAE
  case class App(funExpr: KCFLAE, arg: KCFLAE)                extends KCFLAE
  case class If0(testE: KCFLAE, thenE: KCFLAE, elseE: KCFLAE) extends KCFLAE
  case class Shift(name: String, body: KCFLAE)                extends KCFLAE
  case class Reset(body: KCFLAE)                              extends KCFLAE

  case class Closure(run: Continuation2 => Continuation2)
  type Value = Num | Closure

  type Continuation  = Value => Value
  type Continuation2 = Continuation => Continuation
  type Env           = Map[String, Value]

  case class CPS(run: (Continuation, Continuation2) => Value):
      def flatMap[Y](f: Value => CPS): CPS =
        // first read ShiftReset
        // now recognise the pattern:
        // - pass envk1 in, get envk2 out;
        // - call f on the value x
        // - pass envk2 in
        // - k always at the very end
        // e.g., the continuation is threaded through the interpreter,
        // while enlargening the metacontinuation with the current function f
        CPS((envk1, k) => this.run(envk1, envk2 => x => f(x).run(envk2, k)))
      def map(f: Value => Value): CPS =
        this.flatMap(CPS.pure.compose(f))
  object CPS:
      // in the end, the value and the continuation is always passed to the metacontinuation
      def pure(x: Value): CPS =
        CPS((envk, k) => k(envk)(x))

  /** Little helper for easier evaluation */
  def interp(e: KCFLAE): Value =
      // the expression is interpreted into a CPS value
      val tmp = interp(e, Map.empty)
      // the value of the CPS is extracted by running it
      // with the identity continuation and identity metacontinuation
      tmp.run((x: Value) => x, (x: Continuation) => x)

  /** Abstract over left associative number operations */
  def binOp(verb: String, env: Env, op: (Int, Int) => Int, lhs: KCFLAE, rhs: KCFLAE): CPS =
    for
        lv <- interp(lhs, env)
        rv <- interp(rhs, env)
    yield
      // println((lv, rv))
      (lv, rv) match
          case (Num(n1), Num(n2)) =>
            Num(op(n1, n2))
          case _ =>
            sys.error(s"Can only $verb numbers, but got $lv and $rv")

  // Instead of returning a value, the result is passed to the continuation k.
  def interp(expr: KCFLAE, env: Env): CPS = expr match {
    // for the cases without subexpressions,
    // we directly return a pure CPS value
    case Num(n) =>
      CPS.pure(Num(n))
    case Id(name) =>
      CPS.pure(env(name))
    case Fun(arg, body) =>
      CPS.pure(Closure((k: Continuation2) =>
        (envk: Continuation) =>
          (v: Value) =>
            interp(body, env + (arg -> v)).run(envk, k)
      ))

    // in the cases with subexpressions,
    // we sequentially go through each subexpression
    case Add(lhs, rhs)                 => binOp("add", env, _ + _, lhs, rhs)
    case Sub(lhs, rhs)                 => binOp("sub", env, _ - _, lhs, rhs)
    case Mult(lhs, rhs)                => binOp("mul", env, _ * _, lhs, rhs)
    case Let(boundId, namedExpr, body) =>
      for
          nv  <- interp(namedExpr, env)
          res <- interp(body, env + (boundId -> nv))
      yield res

    case App(funExpr, argExpr) =>
      for
          funValue <- interp(funExpr, env)
          argValue <- interp(argExpr, env)
          result   <- funValue match
              case Closure(clos) =>
                CPS((envk, k) => clos(k)(envk)(argValue))
              case Num(n) =>
                sys.error(s"Can only apply closures or continuations, but got $funValue")
      yield result

    case If0(c, t, e) =>
      for
          res <- interp(c, env)
          tmp <- interp(
            res match {
              case Num(0) => t
              case _      => e
            },
            env
          )
      yield tmp

    // reset merges the metacontinuation k into the continuation envk
    // (setting the new metacontinuation to identity)
    case Reset(body) =>
      CPS((envk, k) =>
        interp(body, env).run(k(envk), identity)
      )

    // shift moves the metacontinuation to the given variable name
    // (setting the new metacontinuation to identity)
    case Shift(name, body) =>
      CPS((envk, k1) =>
          val valu = Closure(k2 => k1.compose(k2))
          interp(body, env + (name -> valu)).run(envk, identity)
      )

  }
}
