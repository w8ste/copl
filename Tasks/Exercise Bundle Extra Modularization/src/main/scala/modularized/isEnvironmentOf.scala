package modularized

infix trait isEnvironmentOf[E, V] {
  extension (env: E)
      def lookup(id: String): V
      def apply(id: String): V = lookup(id)
      def updated(id: String, value: V): E
}
