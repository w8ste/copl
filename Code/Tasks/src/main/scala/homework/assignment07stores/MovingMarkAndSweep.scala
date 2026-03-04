package homework.assignment07stores

import interpreters.memory.stores.{MarkAndSweepStore, VirtualMemoryStore}

type Location = Int

/* The mark&sweep GC requires to inspect values. To simplify the API a bit, we only support the GC.Value in the companion object.
 * However, the class itself is fully generic and works with any value, for which a fitting function to extract locations is provided */
object MovingMarkAndSweep {
  def apply[V](maxSize: Int, locationsInValue: V => Set[Location]): MovingMarkAndSweep[V] =
    new MovingMarkAndSweep(Seq.fill(maxSize)(None), Map.empty, locationsInValue, false)
}

class MovingMarkAndSweep[Val](
    memory: Seq[Option[Val]],
    virtualMemory: Map[Location, Location],
    locationsInValue: Val => Set[Location],
    alwaysCollectGarbage: Boolean,
) extends MarkAndSweepStore[Val](memory, virtualMemory, locationsInValue, alwaysCollectGarbage) {

  override def withMemory(
      memory: Seq[Option[Val]],
      virtual: Map[Location, Location] = virtualMemory
  ): MovingMarkAndSweep[Val] =
    new MovingMarkAndSweep(memory, virtual, locationsInValue, alwaysCollectGarbage)

  override def gc(seed: Set[Location]): VirtualMemoryStore[Val] =
      val marked = mark(seed)
      sweep(marked).compact()

  // TASK1: update the function to actually do compaction!

  override def compact(): VirtualMemoryStore[Val] =
    this

}
