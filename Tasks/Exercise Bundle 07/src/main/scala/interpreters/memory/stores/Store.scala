package interpreters.memory.stores

/* To be able to experiment with different store and gc designs, we
 * create an interface for stores. The stack parameter in malloc is
 * needed during gc to determine the root nodes from which the
 * algorithms can start.
 */

/* This model of garbage collection does not illustrate the difficulty
 * of memory management. In most languages, the size of the allocated
 * memory regions on the heap vary, and hence one needs an algorithm
 * to find a free and large-enough spot on the heap. There are various
 * algorithms and heuristics (best-fit, worst-fit, first-fit, ...) for
 * that purpose.
 *
 * There are also various alternative gc designs. Mark-and-sweep is a
 * non-moving algorithm, where reachable heap objects are never moved.
 * In contrast to that, copying gc algorithms move the reachable
 * objects to a different portion of the heap. One of the oldest
 * algorithms is the semi-space garbage collector, in particular with
 * the implementation purpose.

     http://www.cs.umd.edu/class/fall2002/cmsc631/cheney/cheney.html

 * Topic for class discussion: What are the pros and cons of moving
 * vs. non-moving gc?
 *
 * It can be shown empirically that most unreachable objects become
 * unreachable while they are still young. Generational gc algorithms
 * take this empirical fact into account and divide the objects into
 * generations, whereby the (small) youngest generation of objects is
 * garbage-collected more frequently.
 *
 * A typical problem of the simple gc algorithms we discussed is the
 * stop-the-world phenomenon: All execution has to be stopped during a
 * gc cycle. This issue is addressed by incremental or concurrent
 * garbage collectors. Incremental garbage collectors typically reduce
 * the total throughput but increase responsiveness and real-time
 * behavior.
 *
 * A completely different approach to memory management is _reference
 * counting_. In reference counting, each object on the heap (in our
 * case, each box) maintains a counter which says how many pointers
 * currently point to that object. The counter is adjusted whenever a
 * pointer variable is assigned to this object (incremented), or from
 * this object to another object (decremented). When the counter is 0,
 * the object can be reclaimed.
 *
 * The obvious disadvantage of reference counting is that it cannot
 * detect cycles on the heap. Hence reference counting algorithm must
 * be augmented with some means to detect cycles.
 *
 * Topic for class discussion: What are the pros and cons of reference
 * counting vs. tracing garbage collectors such as mark-and-sweep or
 * semi-space?
 */

type Location = Int

trait Store[Val] {

  /** malloc may trigger garbage collection, thus it requires the set of roots */
  def malloc(v: Val, rootLocations: Set[Location]): (Location, Store[Val])
  def update(index: Location, v: Val): Store[Val]
  def lookup(index: Location): Val

  // for weak references and testing
  def contains(index: Location): Boolean

  // used for testing
  def remainingCapacity: Int
  def gc(seed: Set[Location]): Store[Val]
}
