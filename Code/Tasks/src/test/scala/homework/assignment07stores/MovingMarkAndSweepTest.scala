package homework.assignment07stores

import interpreters.memory.GarbageCollection.*
import interpreters.memory.RootedEnv
import interpreters.memory.stores.VirtualMemoryStore
import munit.FunSuite

import scala.language.implicitConversions

class MovingMarkAndSweepTest extends FunSuite {

  implicit def idToSCFLAE(id: String): Id                        = Id(id)
  implicit def numToSCFLAE(n: Int): Num                          = Num(n)
  implicit def rootedMap(dict: Map[String, Location]): RootedEnv = RootedEnv(dict, dict.values.toSet)

  def interp2(
      expr: StoreExpr,
      stack: RootedEnv = RootedEnv.empty
  ): Value = {
    val (v, s) = interp(expr, stack, MovingMarkAndSweep(100, locationsInValue))
    v
  }

  val garbage: Value = Num(-42)
  val v: Value       = Num(100)

  def emptyEnv: RootedEnv = RootedEnv.empty

  def emptyStack: List[RootedEnv] = emptyEnv :: Nil

  class StoreTester(maxSize: Int) {
    private var store: VirtualMemoryStore[Value] = MovingMarkAndSweep(maxSize, locationsInValue)

    def addGarbage(): Unit =
      store = store.malloc(garbage, Set.empty)._2

    def addValue(value: Value, stack: RootedEnv = RootedEnv.empty): Location = {
      val (location, st) = store.malloc(value, stack.roots)
      store = st
      location
    }

    def runGC(stack: List[Map[String, Location]])(implicit loc: munit.Location): Unit = {
      store = store.gc(stack.flatMap(env => env.values).toSet)
      val (used, free) = store.memory.span(elem => elem.isDefined)
      assert(free.forall(_.isEmpty), s"expected store to be compacted, but $free contains values")
    }

    def getStore: VirtualMemoryStore[Value] = store

  }

  test("compacts on gc") {
    val st = new StoreTester(10)
    st.addGarbage()
    st.addGarbage()
    val vLoc = st.addValue(v)
    st.addGarbage()
    st.runGC(Map("x" -> vLoc) :: Nil)
    assertEquals(st.getStore.lookup(vLoc), v, s"GC does not compact ($vLoc, ${st.getStore.virtualMemory}")
  }

  test("updates box references on gc") {
    val st = new StoreTester(10)
    st.addGarbage()
    val targetLoc = st.addValue(v)
    st.addGarbage()
    val boxLoc = st.addValue(Box(targetLoc))
    st.runGC(Map("b" -> boxLoc) :: Nil)
    assertEquals(st.getStore.lookup(boxLoc), Box(1))
    assertEquals(st.getStore.lookup(1), v)
  }

  test("updates closure environments on gc") {
    val st = new StoreTester(10)
    st.addGarbage()
    st.addGarbage()
    val targetLoc = st.addValue(v)
    st.addGarbage()
    val closureLoc = st.addValue(Closure("_", "_", Map("x" -> targetLoc)))
    st.runGC(Map("f" -> closureLoc) :: Nil)
    assertEquals(st.getStore.lookup(closureLoc), Closure("_", "_", Map("x" -> 2)))
    assertEquals(st.getStore.lookup(2), v)
  }

  test("updates stack on gc") {
    val st = new StoreTester(10)
    st.addGarbage()
    st.addGarbage()
    val vLoc = st.addValue(v)
    st.addGarbage()
    val stack = Map("x" -> vLoc) :: Nil
    st.runGC(stack)
    assertEquals(st.getStore.lookup(vLoc), v)
  }

  test("sample from description works") {
    val st = new StoreTester(6)
    st.addValue(Num(23))
    st.addValue(Num(42))
    st.addValue(Box(1))
    st.addValue(Box(2))
    st.addValue(Num(-1))
    st.addValue(Closure("_", "_", Map("x" -> 4)))
    val stack = Map("b" -> 2, "f" -> 5) :: Nil
    st.runGC(stack)
    assertEquals(st.getStore.lookup(1), Num(42))
    assertEquals(st.getStore.lookup(2), Box(1))
    assertEquals(st.getStore.lookup(4), Num(-1))
    assertEquals(st.getStore.lookup(5), Closure("_", "_", Map("x" -> 4)))
  }

  test("nextFreeAddr points at correct position after gc") {
    val st = new StoreTester(6)
    st.addValue(Num(23))
    st.addValue(Num(42))
    st.addValue(Box(1))
    st.addValue(Box(2))
    st.addValue(Num(-1))
    st.addValue(Closure("_", "_", Map("x" -> 4)))
    val stack = Map("b" -> 2, "f" -> 5) :: Nil
    st.runGC(stack)
    assertEquals(st.getStore.nextFreeLocation, Some(4))
  }

  test("does not erroneously reset object if position keeps the same") {
    val st = new StoreTester(6)
    st.addValue(Num(23))
    st.addValue(Box(0))
    st.addValue(Box(1))
    val stack = Map("b" -> 2) :: Nil
    st.runGC(stack)
    assertEquals(st.getStore.lookup(0), Num(23))
  }

  test("correctly compacts Let forward references") {
    val st = new StoreTester(6)
    st.addValue(Num(23))
    st.addValue(Box(2))
    st.addValue(Num(42))
    val stack = Map("b" -> 1) :: Nil
    st.runGC(stack)
    assert(st.getStore.memory(0).get.isInstanceOf[Box])
    assertEquals(st.getStore.memory(1).get, Num(42))
  }

  test("correctly updates forward references in store") {
    val st = new StoreTester(6)
    st.addGarbage()
    st.addValue(Box(3))
    st.addGarbage()
    st.addValue(Num(42))
    val stack = Map("b" -> 1) :: Nil
    st.runGC(stack)
    assertEquals(st.getStore.memory(0).get, Box(3))
    assertEquals(st.getStore.lookup(3), Num(42))
  }

  test("correctly updates forward references in stack") {
    val st = new StoreTester(6)
    st.addValue(Num(23))
    st.addValue(Box(2))
    st.addValue(Num(42))
    val stack = Map("b" -> 1) :: Nil
    st.runGC(stack)
    assertEquals(stack, Map("b" -> 1) :: Nil)
    assertEquals(st.getStore.memory(0).get, Box(2))
    assertEquals(st.getStore.lookup(2), Num(42))
  }

  test("no values are garbage collected or changed") {
    val st = new StoreTester(6)
    st.addValue(Num(23))
    st.addValue(Box(0))
    st.addValue(Num(42))
    val stack = Map("b" -> 1, "n" -> 2) :: Nil
    st.runGC(stack)
    assertEquals(stack, Map("b" -> 1, "n" -> 2) :: Nil)
    assertEquals(st.getStore.lookup(0), Num(23))
    assertEquals(st.getStore.lookup(1), Box(0))
    assertEquals(st.getStore.lookup(2), Num(42))
  }

  test("gc during malloc is handled correctly") {
    val st = new StoreTester(2)
    st.addGarbage()
    val numL = st.addValue(Num(999))
    val boxL = st.addValue(Box(numL))
    // println(st.getStore)
    val box = st.getStore.lookup(boxL)
    assert(box.isInstanceOf[Box])
    assertEquals(st.getStore.lookup(box.asInstanceOf[Box].location), Num(999))
  }

  test("test6") {
    assertEquals(
      interp2(
        Let("b", 0, If0(Seqn(SetId("b", 5), "b"), 1, "b"))
      ),
      Num(5)
    )
  }

  test("test7") {
    assertEquals(interp2(Let("b", 4, Add("b", Seqn(SetId("b", 5), "b")))), Num(9))
  }
}
