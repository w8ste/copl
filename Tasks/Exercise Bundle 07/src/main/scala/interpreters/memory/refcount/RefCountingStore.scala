package interpreters.memory.refcount

import interpreters.memory.refcount.ReferenceCounted.{Box, Closure, NilV, Num}
import interpreters.memory.stores.Store

case class RefCountingStore[Val](maxSize: Int, memory: Map[Int, (Val, Int)] = Map.empty) extends Store[Val] {

  override def gc(seed: Set[Location]): Store[Val] = this
  override def remainingCapacity: Location         = maxSize - memory.size

  val size: Location = memory.size // .count(x => x != null)

  override def malloc(v: Val, rootLocations: Set[Location]): (Location, RefCountingStore[Val]) = {
    val loc = getFreeAddr
    Debug.debug(s"assigning $v to new $loc")
    (loc, copy(memory = memory + (loc -> (v, 1))))
  }

  def contains(index: Location): Boolean = memory.contains(index)

  /** makes a shallow copy of a value increasing all things it refers to */
  def shallowCopy(locVal: Val): RefCountingStore[Val] = {
    locVal match {
      case Num(_)             => this
      case Box(boxLoc)        => enref(boxLoc)
      case Closure(_, _, env) =>
        // we made a copy of a closure, so we increase the refcount of every location pointed to by the environment of the closure by one
        env.foldLeft(this) {
          case (store, (envSym, envLoc)) => store.enref(envLoc)
        }
    }
  }

  /** duplicates the value at a location to a new location */
  def copyLocation(loc: Location): (Location, RefCountingStore[Val]) = {
    val freeLoc = getFreeAddr
    val locVal  = lookup(loc)

    Debug.debug(s"copy $locVal from $loc to $freeLoc")

    // Copy the value in the store
    val copyStore = copy(memory = memory + (freeLoc -> (locVal, 1)))

    // Enref values referenced by the copied value
    val storeWithEnref = copyStore.shallowCopy(locVal)

    (freeLoc, storeWithEnref)
  }

  def update(index: Location, v: Val): RefCountingStore[Val] = {
    Debug.debug(s"mutating loc $index to $v, was ${memory.get(index)}")
    memory.get(index) match
        case None =>
          val s1 = copy(memory = memory + (index -> (v, 0)))
          s1.shallowCopy(v)
        case Some((oldValue, count)) =>
          val s1 = copy(memory = memory + (index -> (v, count)))
          s1.derefValue(oldValue).shallowCopy(v)
  }

  override def lookup(index: Location): Val =
    memory.get(index) match
        case None =>
          sys.error(s"tried to lookup non present location $index, memory was:\n  $memory")
        case Some((v, _)) => v

  def free(index: Location): RefCountingStore[Val] =
    memory.get(index) match
        case None =>
          throw IllegalStateException(s"freed empty location $index")
        case Some((value, count)) =>
          if count > 0 then IllegalStateException(s"free location $index with refcount $count")
          Debug.debug(s"free $index, was $value")
          val removed = copy(memory = memory - index)
          removed.derefValue(value)

  def enref(index: Location): RefCountingStore[Val] = {
    val oldValue = lookup(index)
    val newCount = refCount(index) + 1
    Debug.debug(s"increasing ref of $index to $newCount (value is $oldValue)")
    copy(memory = memory + (index -> (oldValue, newCount)))
  }

  def derefValue(value: Val): RefCountingStore[Val] = {
    value match {
      case Num(_) | NilV      => this
      case Box(boxedLoc)      => this.deref(boxedLoc)
      case Closure(_, _, env) => env.foldLeft(this) {
          case (store, (envSym, envLoc)) => store.deref(envLoc)
        }
    }
  }

  def deref(index: Location): RefCountingStore[Val] = {
    val oldRefcount = refCount(index)
    Debug.debug(s"decreasing refcount of loc $index, was $oldRefcount")
    oldRefcount match {
      case 0 =>
        // we would like to avoid this, but it can legitimately happen in case of recursive definitions
        Debug.debug(s"decreased refcount of $index below 0")
        this
      case 1     => free(index)
      case other =>
        copy(memory = memory + (index -> (lookup(index), oldRefcount - 1)))
    }
  }

  private def getFreeAddr: Location = {
    val max = memory.keysIterator.maxOption.getOrElse(-1) + 1
    if max < maxSize
    then max
    else
        Range(0, maxSize)
          .find(i => !memory.contains(i))
          .getOrElse(sys.error("out of memory"))
  }

  private def refCount(index: Int): Int = memory.get(index) match {
    case None         => 0
    case Some((_, n)) => n
  }

}
