package interpreters.memory.stores

/** The idea behind a virtual memory store is that some GC move values in the store around, to create contiguous regions of free memory.
  * Having such free regions is advantageous in a more realistic scenario for memory consumption and allocation performance.
  * We will still look at how such moving GCs work, even if there is no practical benefit in our implementation.
  *
  * In any case, a moving GC moves stored values from one location to another. However, this makes all existing references invalid.
  * The way we deal with this, is to provide a “virtual memory mapping” which provides the illusion of infinite locations.
  * Every allocated value receives a new virtual location. This virtual location is then mapped to an actual location in the memory.
  *
  * Note that this implementation does not yet do any garbage collection at all (and no moving either), and is meant for subclasses to override the necessary methods.
  *
  * Also, be careful to always override the `withMemory` method, as otherwise returned copies will not have the correct type :(
  */
class VirtualMemoryStore[Val](
    val memory: Seq[Option[Val]],
    val virtualMemory: Map[Location, Location],
    locationsInValue: Val => Set[Location]
) extends Store[Val] {

  override val remainingCapacity: Int = memory.count(e => e.isEmpty)

  def withMemory(memory: Seq[Option[Val]], virtual: Map[Location, Location] = virtualMemory): VirtualMemoryStore[Val] =
    new VirtualMemoryStore(memory, virtual, locationsInValue)

  def translateVirtual(index: Location): Location =
    virtualMemory.getOrElse(index, index)

  override def update(index: Location, v: Val): VirtualMemoryStore[Val] =
    withMemory(memory.updated(translateVirtual(index), Some(v)))

  def lookup(index: Location): Val =
    memory(translateVirtual(index)).getOrElse(sys.error(s"invalid lookup at $index"))

  def contains(index: Location): Boolean =
    virtualMemory.getOrElse(index, -1) != -1

  def malloc(v: Val, rootLocations: Set[Location]): (Location, VirtualMemoryStore[Val]) =
    gcIfFull(v, rootLocations).mallocNoGc(v)

  private def gcIfFull(v: Val, rootLocations: Set[Location]): VirtualMemoryStore[Val] =
    if remainingCapacity <= 0 then {
      val seed = rootLocations union locationsInValue(v)
      gc(seed)
    } else {
      this
    }

  private def mallocNoGc(v: Val): (Location, VirtualMemoryStore[Val]) = {
    nextFreeLocation match {
      case None           => sys.error("out of memory")
      case Some(location) =>
        // malloc always assigns strictly increasing virtual locations
        val virtualLocation = nextFreeVirtualLocation
        val newStore        =
          withMemory(
            memory = memory.updated(location, Some(v)),
            virtual = virtualMemory + (virtualLocation -> location)
          )
        (virtualLocation, newStore)
    }
  }

  def nextFreeLocation: Option[Location] =
      val res = memory.indexWhere(e => e.isEmpty)
      // indexWhere returns negative numbers when an entry is not found, but we want to use an option instead
      Option.when(res >= 0)(res)

  // does not collect garbage
  override def gc(seed: Set[Location]): VirtualMemoryStore[Val] = this

  def nextFreeVirtualLocation: Location =
    virtualMemory.keys.maxOption.getOrElse(-1) + 1
}
