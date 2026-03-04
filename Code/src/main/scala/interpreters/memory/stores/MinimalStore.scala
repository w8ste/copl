package interpreters.memory.stores

/* Here is one implementation of the Store interface that does not
 * perform gc. It just runs out of memory once the store is full.
 */
case class MinimalStore[Val](
    maxSize: Int,
    memory: Seq[Val] = Seq(),
) extends Store[Val] {

  override def remainingCapacity: Int = maxSize - memory.size

  def malloc(v: Val): (Location, MinimalStore[Val]) = malloc(v, Set.empty)

  def malloc(v: Val, rootLocations: Set[Location]): (Location, MinimalStore[Val]) = {
    if memory.size >= maxSize then sys.error("out of memory")
    (memory.size, add(v))
  }

  def add(v: Val): MinimalStore[Val] = copy(memory = memory :+ v)

  def update(index: Location, v: Val): MinimalStore[Val] = copy(memory = memory.updated(index, v))

  def lookup(index: Location): Val                 = memory(index)
  def contains(index: Location): Boolean           = memory.contains(index)
  override def gc(seed: Set[Location]): Store[Val] = this
}
