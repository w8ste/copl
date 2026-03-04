package interpreters.memory

type Location = Int

/** A rooted env tracks a set of `roots` – locations that are reachable for the purpose of garbage collections.
  * Specifically, this set contains all locations that are somewhere in environments further up the stack.
  */
case class RootedEnv(dict: Map[String, Location], roots: Set[Location]) {
  export dict.{values, apply, get}

  def +(elem: (String, Location)): RootedEnv =
    RootedEnv(dict + elem, roots + elem._2)

  def stacked(roots: Set[Location]): RootedEnv = RootedEnv(dict, this.roots ++ roots)

  def replace(other: Map[String, Location]): RootedEnv = RootedEnv(other, roots ++ other.values)
}

object RootedEnv {
  def empty: RootedEnv = RootedEnv(Map.empty, Set.empty)

  def apply(dict: Map[String, Location]): RootedEnv = RootedEnv(dict, dict.values.toSet)
}
