package interpreters.recursion

// To be very explicit about where mutation happens, we use this “box” abstraction.
// A box contains a value, and there are two types of boxes: mutable and immutable ones.
// We store the box in the environment, to allow us to change the value later.
// Scala also has mutable maps, that directly enable modification, but our interpreter relies on immutable maps to implement scoping (have bindings only be valid within the body of a let, and going back to the old environment once that scope is left).
sealed trait Box[V] { def value: V }
object Box          {
  case class Immutable[V](value: V) extends Box[V]
  class Mutable[V](var value: V)    extends Box[V]

  // Instead of using a mutable value, we can also use Scala’s built-in lazy evaluation to enable ”late binding” of the value.
  // To do so, we pass a function `v` that will produce the value – but we will only evaluate it once the `value` is accessed.
  // This is achieved by using Scala’s `lazy` modifier on vals, which takes care of evaluating the body of the val on first access, and then storing the result for later accesses.
  // Scala’s lazy vals are generally implemented using mutable state on the JVM, so technically one could argue that we still use mutability – though it is not possible to observe the mutation from the Scala side.
  class Lazy[V](v: () => V) extends Box[V] {
    override lazy val value: V = v()
  }
}

object Recursion {

  sealed trait RecursiveCFLAE
  type RCFLAE = RecursiveCFLAE
  case class Num(n: Int)                                           extends RCFLAE
  case class Add(lhs: RCFLAE, rhs: RCFLAE)                         extends RCFLAE
  case class Sub(lhs: RCFLAE, rhs: RCFLAE)                         extends RCFLAE
  case class Mult(lhs: RCFLAE, rhs: RCFLAE)                        extends RCFLAE
  case class Let(name: String, namedExpr: RCFLAE, body: RCFLAE)    extends RCFLAE
  case class Id(name: String)                                      extends RCFLAE
  case class Fun(param: String, body: RCFLAE)                      extends RCFLAE
  case class If0(test: RCFLAE, posBody: RCFLAE, negBody: RCFLAE)   extends RCFLAE
  case class LetRec(name: String, namedExpr: RCFLAE, body: RCFLAE) extends RCFLAE
  case class App(funExpr: RCFLAE, argExpr: RCFLAE)                 extends RCFLAE

  trait Arithmetic {
    // environments are completely generic, notably, the substitution interpreter does not use them
    type Env
    // values must at least include `Num`, but may include more
    type Value >: Num
    // Arithmetic expressions are exactly one of these cases
    type AE = Num | Sub | Add | Mult | If0

    // the generic interp signature
    def interp(expr: RCFLAE, env: Env): Value

    def binOp(lhs: RCFLAE, rhs: RCFLAE, env: Env, combine: (Int, Int) => Int): Num =
      val lv = interp(lhs, env)
      val rv = interp(rhs, env)
      (lv, rv) match {
        case (Num(m), Num(n)) => Num(combine(m, n))
        case _                => sys.error(s"Can do arthmetic with numbers, but got $lhs and $rhs")
      }

    def interpAE(expr: AE, env: Env): Value = {
      expr match {
        case num @ Num(_)                => num
        case Add(lhs, rhs)               => binOp(lhs, rhs, env, _ + _)
        case Sub(lhs, rhs)               => binOp(lhs, rhs, env, _ - _)
        case Mult(lhs, rhs)              => binOp(lhs, rhs, env, _ * _)
        case If0(test, posBody, negBody) =>
          if interp(test, env) == Num(0)
          then interp(posBody, env)
          else interp(negBody, env)
      }
    }
  }

  /** the RCFLAE interpreter with immediate substitution */
  object Substitution extends Arithmetic {

    type Env   = Unit
    type Value = Num | Fun | LetRec

    def subst(expr: RCFLAE, substId: String, value: RCFLAE): RCFLAE = expr match {
      case Num(_) => expr

      case Add(lhs, rhs) =>
        Add(subst(lhs, substId, value), subst(rhs, substId, value))

      case Sub(lhs, rhs) =>
        Sub(subst(lhs, substId, value), subst(rhs, substId, value))

      case Mult(lhs, rhs) =>
        Mult(subst(lhs, substId, value), subst(rhs, substId, value))

      case Let(boundId, namedExpr, boundBody) =>
        val substNamedExpr = subst(namedExpr, substId, value)
        if boundId == substId then
          Let(boundId, substNamedExpr, boundBody)
        else
          Let(boundId, substNamedExpr, subst(boundBody, substId, value))

      case LetRec(boundId, namedExpr, boundBody) =>
        if boundId == substId then expr
        else
          LetRec(boundId, subst(namedExpr, substId, value), subst(boundBody, substId, value))

      case Id(name) =>
        if substId == name then value
        else expr

      case If0(test, posBody, negBody) =>
        If0(subst(test, substId, value), subst(posBody, substId, value), subst(negBody, substId, value))

      case App(funExpr, argExpr) =>
        App(subst(funExpr, substId, value), subst(argExpr, substId, value))

      case Fun(arg, body) =>
        if arg == substId then expr
        else Fun(arg, subst(body, substId, value))

    }

    def interp(expr: RCFLAE, env: Env = ()): Value = expr match {
      case arithmetic: AE => interpAE(arithmetic, env)

      case Let(boundId, namedExpr, boundBody) =>
        interp(subst(boundBody, boundId, interp(namedExpr)))

      case LetRec(boundId, namedExpr, boundBody) =>
        val recExpr              = LetRec(boundId, namedExpr, Id(boundId))
        val transformedNamedExpr = subst(namedExpr, boundId, recExpr)
        interp(subst(boundBody, boundId, interp(transformedNamedExpr)))

      case Id(name) => sys.error("found unbound id " + name)

      case App(funExpr, argExpr) => interp(funExpr) match {
          case Fun(param, body) =>
            interp(subst(body, param, interp(argExpr)))
          case _ => sys.error("Can only handle function expressions")
        }

      case f @ Fun(_, _) => f
    }
  }

  // This object contains an interpreter for RCFLAE with deferred substitution and
  // static scoping. The interpreter employs mutable environments to implement
  // recursion.
  object Environment extends Arithmetic {

    type Env = Map[String, Box[Value]]
    case class Closure(param: String, body: RCFLAE, env: Env)

    type Value = Num | Closure

    def interp(expr: RCFLAE, env: Env = Map()): Value = expr match {

      case other: AE => interpAE(other, env)

      case Let(boundId, namedExpr, boundBody) =>
        interp(boundBody, env + (boundId -> Box.Immutable(interp(namedExpr, env))))

      case Id(name) => env(name).value

      case Fun(arg, body) => Closure(arg, body, env)

      case LetRec(boundId, namedExpr, boundBody) =>
        // create a mutable box containing a dummy value
        val mutableBox = Box.Mutable[Value](Num(-42))
        // create an environment that contains the mutable box
        val recEnv = env + (boundId -> mutableBox)
        // evaluate the expression with the correct environment, but still containing the dummy value
        // it this point, it is not yet possible to call the recursive definition (only to capture it for later)
        val namedValue = interp(namedExpr, recEnv)
        // make the recursive binding happen
        mutableBox.value = namedValue
        interp(boundBody, recEnv)

      case App(funExpr, argExpr) =>
        val funV = interp(funExpr, env)
        funV match {
          case Closure(fParam, fBody, fEnv) =>
            interp(fBody, fEnv concat Map(fParam -> Box.Immutable(interp(argExpr, env))))
          case _ => sys.error("can only apply functions, but got: " + funV)
        }
    }
  }

  // This object contains an interpreter for RCFLAE with deferred substitution and
  // static scoping. The interpreter employs environments with deferred values
  // to implement recursion.
  object ImmutableEnvironment extends Arithmetic {

    // Instead of using a mutable environment (as in the lecture),
    // we store “deferred” computations to values.
    // This allows us to create environments where some values are not yet computed (because of recursion).
    type Env = Map[String, Box[Value]]
    case class Closure(param: String, body: RCFLAE, env: Env)

    type Value = Num | Closure

    def interp(expr: RCFLAE, env: Env = Map()): Value = expr match {

      case other: AE => interpAE(other, env)

      case Let(boundId, namedExpr, boundBody) =>
        val namedValue = interp(namedExpr, env)
        interp(boundBody, env.updated(boundId, Box.Immutable(namedValue)))

      case Id(name) =>
        // the apply is from the function stored within the environment, and thus computes the deferred value.
        env(name).value

      case Fun(arg, body) => Closure(arg, body, env)

      case LetRec(boundId, namedExpr, boundBody) =>
        // A `lazy val` is only evaluated when it is first used (similar to call-by-name)
        // but stores the result of the evaluation.
        // Pragmatically, it allows exactly this kind of recursive value definitions.
        // We also have a dependency between `boundValue` and `recEnv` which is why we use this
        // syntax to define/return both within the same block.
        lazy val (boundValue: Value, recEnv: Env) =
          val recEnv2 = env + (boundId -> Box.Lazy(() => boundValue))
          val v       = interp(namedExpr, recEnv2)
          (v, recEnv2)
        interp(boundBody, recEnv)

      case App(funExpr, argExpr) =>
        val funV = interp(funExpr, env)
        funV match {
          case Closure(fParam, fBody, fEnv) =>
            val argValue = interp(argExpr, env)
            interp(fBody, fEnv.updated(fParam, Box.Immutable(argValue)))
          case _ => sys.error("can only apply functions, but got: " + funV)
        }
    }
  }

  object Meta {

    // This object contains an interpreter for RCFLAE with functional environments.
    object Environment extends Arithmetic {

      case class Closure(param: String, body: RCFLAE, env: Env)

      type Value = Num | Closure

      type Env = String => Value

      def emptyEnv(name: String): Value = sys.error("No binding for " + name)
      def recursiveBindAndInterp(boundId: String, namedExpr: RCFLAE, env: Env): Env = {
        def recEnv: Env = (id: String) =>
          if id == boundId
          then interp(namedExpr, recEnv)
          else env(id)
        recEnv
      }

      def createEnv(name: String, value: Value, oldEnv: Env): Env =
        (id: String) =>
          if id == name
          then value
          else oldEnv(id)

      def interp(expr: RCFLAE, env: Env = emptyEnv): Value = expr match {
        case arithmetic: AE => interpAE(arithmetic, env)

        case Let(boundId, namedExpr, boundBody) =>
          interp(boundBody, createEnv(boundId, interp(namedExpr, env), env))

        case Id(name) => env(name)

        case Fun(arg, body) => Closure(arg, body, env)

        case LetRec(boundId, namedExpr, boundBody) =>
          interp(boundBody, recursiveBindAndInterp(boundId, namedExpr, env))

        case App(funExpr, argExpr) =>
          val funV = interp(funExpr, env)
          funV match {
            case Closure(fParam, fBody, fEnv) =>
              interp(fBody, createEnv(fParam, interp(argExpr, env), fEnv))
            case _ => sys.error("can only apply functions, but got: " + funV)
          }
      }
    }

    // This object contains an interpreter for RCFLAE with deferred substitution and
    // static scoping. The interpreter employs circular environments to implement
    // recursion.
    object Closures extends Arithmetic {

      case class Closure(f: Value => Value)

      type Value = Num | Closure

      // Again: this type signature only makes sense for a strict call-by-name evaluation strategy.
      type Env = Map[String, Box[Value]]

      def interp(expr: RCFLAE, env: Env = Map()): Value = expr match {
        case arithmetic: AE => interpAE(arithmetic, env)

        case Let(boundId, namedExpr, boundBody) =>
          interp(boundBody, env concat Map(boundId -> Box.Immutable(interp(namedExpr, env))))

        case Id(name) => env(name).value

        case Fun(arg, body) =>
          Closure(argval => interp(body, env + (arg -> Box.Immutable(argval))))

        case LetRec(boundId, namedExpr, boundBody) =>
          val mutableBox = Box.Mutable[Value](Num(-42))
          val recEnv     = env + (boundId -> mutableBox)
          val namedValue = interp(namedExpr, recEnv)
          mutableBox.value = namedValue
          interp(boundBody, recEnv)

        case App(funExpr, argExpr) =>
          val funV = interp(funExpr, env)
          val argV = interp(argExpr, env)
          funV match {
            case Closure(f) => f(argV)
            case _          => sys.error(s"can only apply functions, but got: $funV")
          }
      }
    }

    // This object contains an interpreter for RCFLAE with functions and environments
    // represented by Scala functions.
    object EnvironmentAndClosures extends Arithmetic {

      case class Closure(f: Value => Value)

      type Value = Num | Closure

      type Env = String => Value

      def emptyEnv(name: String): Value = sys.error("No binding for " + name)

      def createEnv(name: String, value: Value, oldEnv: Env): Env =
        (id: String) => {
          if id == name then value
          else oldEnv(id)
        }

      def cycBindAndInterp(boundId: String, namedExpr: RCFLAE, env: Env): Env = {
        lazy val recEnv: Env = (id: String) => {
          if id == boundId then interp(namedExpr, recEnv)
          else env(id)
        }
        recEnv
      }

      def interp(expr: RCFLAE, env: Env = emptyEnv): Value = expr match {
        case arithmetic: AE => interpAE(arithmetic, env)

        case Let(boundId, namedExpr, boundBody) =>
          interp(boundBody, createEnv(boundId, interp(namedExpr, env), env))

        case Id(name) => env(name)

        case Fun(arg, body) => Closure(argValue => interp(body, createEnv(arg, argValue, env)))

        case LetRec(boundId, namedExpr, boundBody) =>
          interp(boundBody, cycBindAndInterp(boundId, namedExpr, env))

        case App(funExpr, argExpr) =>
          val funV = interp(funExpr, env)
          val argV = interp(argExpr, env)
          funV match {
            case Closure(f) => f(argV)
            case _          => sys.error("can only apply functions, but got: " + funV)
          }
      }

    }

  }

}
