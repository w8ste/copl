package modularized.stateful

import extras.langserver.parser.{Position, PositionedExpr}
import interpreters.memory.RootedEnv
import interpreters.memory.stores.{MarkAndSweepStore, Store}
import modularized.environmental.MostTypesImpl
import modularized.{AllTypes, isEnvironmentOf}

import scala.annotation.tailrec

object GarbageCollected extends Boxes, Functions, Arithmetic, Misc, Records, AlgebraicData, Booleans, AllTypes,
      MostTypesImpl {

  type Value = Num | Closure | Box | Dummy | True.type | False.type | RecordValue | Constructor | Sym

  case class TBox(inner: Type)
  case object TUnspecified

  type Type = TNum | TBox | TUnspecified.type | Error | TBool.type | TFun | TRecord

  type Expr =
    ArithmeticExpr | BoxExpr | FunExpr | Let | Seqn | LetRec | SetId | TypedFun | RecordExpr | AlgebraicDataExpr | Sym | BooleanExpr | PositionedExpr

  type Context[+A] = Step[Store[Value], A]

  override given sequentialContext: Sequential[Context] = stepSequential

  /** This is the implementation of the store API for `State[Store, V]` – the state constructor containing our usual store.
    * The State data type allows us to read/write the values of type store.
    */
  override def store: StoreApi[Value, Context] = new StoreApi[Value, Context] {
    override def malloc(value: Value, roots: Set[Location]): Context[Location] = Step: s =>
        s.malloc(value, roots)

    override def update(location: Location, value: Value): Context[Unit] = Step: s =>
        ((), s.update(location, value))
    override def lookup(location: Location): Context[Value] = Step: s =>
        (s.lookup(location), s)
  }

  override given storeEnv: EnvironmentStoreApi[Value, RootedEnv, Context] =
    new EnvironmentStoreApi[Value, RootedEnv, Context] {
      extension (env: Env) {
        override def bind(name: String, value: Value): Context[RootedEnv] =
          store.malloc(value, env.roots).andThen: loc =>
              Sequential.ret(env + (name -> loc))

        override def lookup(name: String): Step[Store[Value], Value] =
          store.lookup(env.apply(name))
        override def stack(other: RootedEnv): RootedEnv =
          env.stacked(other.roots)

        override def reachable(other: Value): Env =
          env.stacked(locationsInValue(other))

        override def update(name: String, value: Value): Step[Store[Value], RootedEnv] =
          store.update(env.apply(name), value).andThen: _ =>
              Sequential.ret(env)
      }
    }

  /** A garbage collector must inspect the values in the store to find any references to further locations.
    * The actual implementation below is generic about the value that is stored, so requires a function that does resolve the generic values to actual locations.
    * This is such a function for the specific interpreter Values we are going to use
    */
  def locationsInValue(v: GarbageCollected.Value): Set[Location] = v match {
    case GarbageCollected.Box(a)                 => Set(a)
    case GarbageCollected.Num(_)                 => Set()
    case GarbageCollected.Closure(_, _, env)     => env.values.toSet
    case GarbageCollected.Dummy()                => Set()
    case GarbageCollected.True                   => Set()
    case GarbageCollected.False                  => Set()
    case GarbageCollected.RecordValue(fields)    => fields.values.flatMap(locationsInValue).toSet
    case GarbageCollected.Constructor(_, values) => values.flatMap(locationsInValue).toSet
    case GarbageCollected.Sym(_)                 => Set()
  }

  def interp(expr: Expr): Value =
      val (value, store) = interpStore(expr)
      value

  def interpStore(expr: Expr): (Value, Store[Value]) =
    interp(expr, RootedEnv.empty).run(MarkAndSweepStore(1000, locationsInValue))

  @tailrec
  override def interp(expr: Expr, env: Env): Context[Value] = expr match
      case e: BooleanExpr          => interpBooleans(e, env)
      case e: BoxExpr              => interpBoxes(e, env)
      case e: ArithmeticExpr       => interpArithmetic(e, env)
      case e: MiscExpr             => interpMisc(e, env)
      case TypedFun(p, _, b)       => interpFunctions(Fun(p, b), env)
      case e: (FunExpr | Let)      => interpFunctions(e, env)
      case e: RecordExpr           => interpRecords(e, env)
      case e: AlgebraicDataExpr    => interpAlgebraicData(e, env)
      case e: Sym                  => Sequential.ret(e)
      case Position(e: Expr, _, _) => interp(e, env)

  type Env = RootedEnv

  given locationEnvironment: (Env isEnvironmentOf Location) with {
    extension (env: RootedEnv)
        override def lookup(id: String): Location                    = env.dict.apply(id)
        override def updated(id: String, value: Location): RootedEnv =
          RootedEnv(env.dict.updated(id, value), env.roots + value)
  }

  override given rootedEnvironment: HasRoots[RootedEnv] = new HasRoots[RootedEnv] {
    extension (e: RootedEnv)
        override def roots: Set[Location] = e.roots
  }
}
