package interpreters.memory.stores

object MarkAndSweepStore {

  def apply[V](maxSize: Int, locationsInValueFun: V => Set[Location], alwaysGc: Boolean = true): MarkAndSweepStore[V] =
    new MarkAndSweepStore(Seq.fill(maxSize)(None), Map.empty, locationsInValueFun, alwaysGc)

  def apply[V](size: Int, memory: Seq[Option[V]], locationsInValue: V => Set[Location]) =
    new MarkAndSweepStore[V](memory, Map.empty, locationsInValue, true)
}

class MarkAndSweepStore[Val](
    memory: Seq[Option[Val]],
    virtualMemory: Map[Location, Location],
    locationsInValue: Val => Set[Location],
    alwaysCollectGarbage: Boolean,
) extends VirtualMemoryStore[Val](memory, virtualMemory, locationsInValue, alwaysCollectGarbage) {

  override def withMemory(memory: Seq[Option[Val]], virtual: Map[Location, Location]): MarkAndSweepStore[Val] =
    new MarkAndSweepStore(memory, virtual, locationsInValue, alwaysCollectGarbage)

  override def gc(seed: Set[Location]): VirtualMemoryStore[Val] =
      // println(s"doing it!")
      val marked = mark(seed)
      sweep(marked).compact()

  // Mark: returns all reachable locations starting from a given seed
  // mark works purely on virtual locations
  def mark(seed: Set[Location]): Set[Location] = {
    def markInner(
        markedLocations: Set[Location],
        nextLocations: Set[Location]
    ): Set[Location] = {
      val discoveredLocations = nextLocations.flatMap(i => locationsInValue(lookup(i)))
      val newLocations        = discoveredLocations `diff` markedLocations
      if newLocations.nonEmpty then
          markInner(markedLocations `union` newLocations, newLocations)
      else
          markedLocations
    }

    markInner(seed, seed)
  }

  // Sweep: Delete all values that are not in the marked locations
  def sweep(markedLocations: Set[Location]): MarkAndSweepStore[Val] = {
    // sweep is passed virtual locations, but then translates them to physical locations to do the actual compaction
    val markedPhysical = markedLocations.map(translateVirtual)
    val sweptMemory    = memory.zipWithIndex.map { (value, index) =>
      if markedPhysical.contains(index)
      then value
      else None
    }

    val sweptVirtual = virtualMemory.map { (v, p) =>
      if markedLocations.contains(v)
      then v -> p
      else v -> -1
    }

    withMemory(
      sweptMemory,
      sweptVirtual
    )
  }

  // implemented in an excercise
  def compact(): VirtualMemoryStore[Val] = this
}
