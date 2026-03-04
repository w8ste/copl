// This file contains an interpreter for SCFLAE with recursive first-class functions, conditionals, mutable boxes, variables and sequencing.

/*
 * Based on the lecture notes for the "Programming Languages and Types"
 * course by Klaus Ostermann at the University of Marburg.
 */
package interpreters.memory.refcount

import interpreters.memory.RootedEnv

type Location = Int

/** RM in 2023: I am pretty sure the whole strategy to present reference counting here is not only buggy (we fixed a lot in the past, but some issues run deep …) but also not really how reference counting realistically works.
  * What this interpreter seems to do is to track reference counts for everything, in particular all of the stack/env copies which is incredibly error prone.
  *
  * I think the core of the issue is related to meta interpretation.
  * We usually use GC in Scala, but to make reference counting work, we need to know when values are no longer referenced,
  * thus we have this whole architecture where we need to track everything in the store,
  * including the values passed around in the interpreter.
  * This overall essentially boils down to doing manual memory management in Scala, which is just super annoying.
  *
  * The need to do the manual memory management is also the reason why the interpreter returns a location – we also allocated all of the Scala values (the return values) in our interpreter store so we can keep track of them being referenced.
  */
object ReferenceCounted {

  sealed trait SRCFLAE
  case class Num(n: Int)                                             extends SRCFLAE
  case class Add(lhs: SRCFLAE, rhs: SRCFLAE)                         extends SRCFLAE
  case class Mult(lhs: SRCFLAE, rhs: SRCFLAE)                        extends SRCFLAE
  case class Let(name: String, namedExpr: SRCFLAE, body: SRCFLAE)    extends SRCFLAE
  case class Id(name: String)                                        extends SRCFLAE
  case class If0(test: SRCFLAE, posBody: SRCFLAE, negBody: SRCFLAE)  extends SRCFLAE
  case class Fun(param: String, body: SRCFLAE)                       extends SRCFLAE
  case class LetRec(name: String, namedExpr: SRCFLAE, body: SRCFLAE) extends SRCFLAE
  case class App(funExpr: SRCFLAE, argExpr: SRCFLAE)                 extends SRCFLAE
  case class Seqn(e1: SRCFLAE, e2: SRCFLAE)                          extends SRCFLAE
  case class SetId(id: String, valueExpr: SRCFLAE)                   extends SRCFLAE
  case class NewBox(valExpr: SRCFLAE)                                extends SRCFLAE
  case class SetBox(boxExpr: SRCFLAE, valueExpr: SRCFLAE)            extends SRCFLAE
  case class OpenBox(boxExpr: SRCFLAE)                               extends SRCFLAE

  type Env = Map[String, Location]

  case object NilV
  case class Closure(param: String, body: SRCFLAE, env: Env)
  case class Box(location: Location)

  type Value = NilV.type | Num | Closure | Box

  def interp(
      expr: SRCFLAE,
      stack: RootedEnv = RootedEnv.empty,
      store: RefCountingStore[Value] = new RefCountingStore[Value](maxSize = 100, memory = Map.empty),
      debug: Debug.Debug = Debug.Debug(0)
  ): (Location, RefCountingStore[Value]) = {

    debug(s"interpreting:\n  $expr\n  $stack\n  $store")
    val disc = Debug.discrepancy(None, store, stack)
    if disc.nonEmpty then
        debug(s"discrepancy: ${disc}")
        // throw IllegalStateException(s"discrepancy in counts detected")

    val res = expr match {

      case Num(n) =>
        /* Allocate a num value and reference it */
        store.malloc(Num(n), stack.roots)

      case Add(lhs, rhs) =>
        val (lloc, store2) = interp(lhs, stack, store, debug.inc)
        val (rloc, store3) = interp(rhs, stack, store2.deref(lloc), debug.inc)

        (store2.lookup(lloc), store3.lookup(rloc)) match {
          case (Num(n1), Num(n2)) =>
            val store4 = store3.deref(rloc)
            store4.malloc(Num(n1 + n2), stack.roots)

          case x => sys.error(s"expected two numbers but got $x")
        }

      case Mult(lhs, rhs) =>
        val (lloc, store2) = interp(lhs, stack, store, debug.inc)
        val (rloc, store3) = interp(rhs, stack, store2.deref(lloc), debug.inc)

        (store2.lookup(lloc), store3.lookup(rloc)) match {
          case (Num(n1), Num(n2)) =>
            val store4 = store3.deref(rloc)
            store4.malloc(Num(n1 * n2), stack.roots)

          case x => sys.error(s"expected two numbers but got $x")
        }

      case Let(boundId, namedExpr, boundBody) =>
        val (namedLoc, store1) = interp(namedExpr, stack, store, debug.inc)
        // Let in this interpreter generates mutable variables, thus we copy the value to have it stored at a different location
        val (copiedLoc, store2) = store1.copyLocation(namedLoc)
        val store2b             = store2.deref(namedLoc)
        val (loc, store3)       = interp(boundBody, stack + (boundId -> copiedLoc), store2b, debug.inc)
        debug(
          s"let ended with loc: $loc, copied: $copiedLoc (named: $namedLoc), store3: \n  $store3\nstore2b: \n  $store2b\nstore2: \n  $store2\nstore1: \n  $store1\nstore: \n  $store"
        )
        (loc, store3.deref(copiedLoc))

      case Id(name) =>
        val loc = stack(name)
        (loc, store.enref(loc))

      case Fun(arg, body) =>
        val closure = Closure(arg, body, stack.dict)
        // increase refcount because we captured references to locations in the closure
        val store1 = stack.dict.foldLeft(store) {
          case (s, (envSym, envLoc)) => s.enref(envLoc)
        }
        store1.malloc(closure, stack.roots)

      case App(funExpr, argExpr) =>
        val (funLoc, s1) = interp(funExpr, stack, store, debug.inc)
        val (argLoc, s2) = interp(argExpr, stack, s1.deref(funLoc), debug.inc)

        s1.lookup(funLoc) match {
          case Closure(fParam, fBody, fEnv) =>
            // Similar to the Let-case, we have to copy the value since it is mutable
            val (copiedLoc, s3) = s2.copyLocation(argLoc)
            // we copy the environment of the closure to the current environment, so we have to increase the references
            val addedEnv  = fEnv.removed(fParam)
            val s3b       = addedEnv.map(_._2).foldLeft(s3.deref(argLoc))((s, loc) => s.enref(loc))
            val (loc, s4) = interp(fBody, stack.replace(addedEnv + (fParam -> copiedLoc)), s3b, debug.inc)
            // and removing the environment again
            val s4b = addedEnv.map(_._2).foldLeft(s4.deref(copiedLoc))((s, loc) => s.deref(loc))
            (loc, s4b)
          case x => sys.error("can only apply functions, but got: " + x)
        }

      case If0(testExpr, thenExpr, elseExpr) =>
        val (testLoc, s1) = interp(testExpr, stack, store, debug.inc)
        val (loc, s2)     = s1.lookup(testLoc) match {
          case Num(0) => interp(thenExpr, stack, s1.deref(testLoc), debug.inc)
          case Num(_) => interp(elseExpr, stack, s1.deref(testLoc), debug.inc)
          case x      => sys.error("can only test numbers, but got: " + x)
        }
        (loc, s2)

      case LetRec(boundId, namedExpr, boundBody) =>
        // allocate a new empty location and make it available in the current environment
        val (newLoc, s2) = store.malloc(NilV, stack.roots)
        val extStack     = stack + (boundId -> newLoc)
        // we interp named expression with the extended environment, so it can capture where to find the location
        // (even though the location only contains NilV, so a lookup at this point would fail;
        // that’s fine: lookup should only happen when the recursive function is called)
        val (namedLoc, s3) = interp(namedExpr, extStack, s2, debug.inc)
        // now we close the loop by modifying the location to contain the actual recursive value
        val loopedStore = s3.update(newLoc, s3.lookup(namedLoc)).deref(namedLoc)
        val (loc, s5)   = interp(boundBody, extStack, loopedStore, debug.inc)
        (loc, s5.deref(newLoc))

      case Seqn(e1, e2) =>
        val (l1, s1) = interp(e1, stack, store, debug.inc)
        interp(e2, stack, s1.deref(l1), debug.inc)

      case NewBox(boxExpr) =>
        val (boxL, boxStore) = interp(boxExpr, stack, store, debug.inc)
        val res              = boxStore.malloc(Box(boxL), stack.roots)
        res

      case SetBox(boxExpr, valueExpr) =>
        val (boxLoc, s1) = interp(boxExpr, stack, store, debug.inc)
        s1.lookup(boxLoc) match {
          case Box(loc) =>
            val (valLoc, s2) = interp(valueExpr, stack, s1.deref(boxLoc), debug.inc)
            val value        = s2.lookup(valLoc)
            // we remove references of whatever was stored in the box location before overwriting
            val s3 = s2.update(loc, value).deref(valLoc)
            (loc, s3.enref(loc))
          case x => sys.error(s"can only set to boxes, but got: $x")
        }

      case OpenBox(boxExpr) =>
        val (boxLoc, s1) = interp(boxExpr, stack, store, debug.inc)
        s1.lookup(boxLoc) match {
          case Box(loc) =>
            // copy the box values so they are not freed when the box goes out of scope
            val (copiedLoc, s2) = s1.copyLocation(loc)
            (copiedLoc, s2.deref(boxLoc))
          case x => sys.error("can only open boxes, but got: " + x)
        }

      case SetId(id, valExpr) =>
        val (valLoc, s1) = interp(valExpr, stack, store, debug.inc)
        val idLoc        = stack(id)
        val value        = s1.lookup(valLoc)
        (idLoc, s1.deref(valLoc).update(idLoc, value).enref(idLoc))
    }

    debug(s"evaluated:\n  $expr\n  ${res._1}\n  ${stack}\n  ${res._2}")
    val disc2 = Debug.discrepancy(Some(res._1), res._2, stack)
    if disc2.nonEmpty then
        debug(s"discrepancy2: ${disc2}")
        // throw IllegalStateException(s"discrepancy in counts detected")
    res
  }

  def main(args: Array[String]): Unit = {

    import scala.language.implicitConversions
    implicit def idToSCFLAE(id: String): Id = Id(id)
    implicit def numToSCFLAE(n: Int): Num   = Num(n)

    val expr = Let("x", NewBox(0), Let("y", NewBox("x"), SetBox("x", "y")))
    println(expr)
    println(interp(expr, RootedEnv.empty))
  }
}

/** Implementing refcount more or less requires manual memory management of the Scala values, which is quite hard to keep track.
  * This object contains tooling to check if the current store seems plausible at every interpretation step.
  */
object Debug {

  import ReferenceCounted.*

  def debug(str: String) =
    // println(str)
    ()

  case class Debug(level: Int) {
    def apply(str: String): Unit =
      debug(str.linesIterator.map(l => " ".repeat(level * 2).appendedAll(l)).mkString("\n"))
    def inc: Debug = Debug(level + 1)
  }

  def references(arg: (Location, (Value, Int))): List[Int] = {
    val (location, (value, count)) = arg
    value match
        case NilV | Num(_)      => Nil
        case Box(location)      => List(location)
        case Closure(_, _, env) => env.valuesIterator.toList
    // to prevent counting cycles do:
    // .filter(_ != location)
  }

  case class LocationInfo(res: Int, memory: Int, stack: Int, count: Int) {
    def ok: Boolean               = res + memory + stack - count == 0
    override def toString: String =
      s"LocationInfo(res = $res, memory = $memory, stack = $stack, count = $count)"
  }

  /** Note, this is no longer correct, because the stack now tracks the roots as a set, thus we can no longer know true counts (only a lower limit) */
  def discrepancy(
      resultLocation: Option[Location],
      store: RefCountingStore[Value],
      stack: RootedEnv
  ): Map[Location, LocationInfo] =
      def getCount(locations: Iterable[Location]): Map[Location, Int] =
        locations.groupMapReduce(identity)(_ => 1)(_ + _)

      val memoryLocs = getCount(store.memory.iterator.flatMap(references).toList)
      val stackLocs  = getCount(stack.roots)

      val present = store.memory.view.mapValues(_._2).toMap
      (memoryLocs.keySet ++ stackLocs.keySet ++ present.keySet).map { key =>
        key -> LocationInfo(
          if resultLocation.contains(key) then 1
          else 0,
          memoryLocs.getOrElse(key, 0),
          stackLocs.getOrElse(key, 0),
          present.getOrElse(key, 0)
        )
      }.filter((k, v) => !v.ok).toMap
}
