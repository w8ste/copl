package exercises.ex08objects

object FunctionsToModelObjects {

  class O {
    val f: Int            = 2
    def add1(x: Int): Int = x + this.f
  }

  class O2 extends O {
    override val f: Int   = 3
    def sub1(x: Int): Int = x - 1
  }

  // Obj = String => Obj => _ => _
  // An object takes a method identifier, a "this" object, and returns a function.
  // The function represents a method or immutable field.
  type Obj = String => ? => ? => ?

  val obj1: Obj = (m: String) =>
    m match {
      case "f"    => (ths: Obj) => (x: Unit) => 2
      case "add1" => (ths: Obj) =>
          (x: Int) =>
            x + ths("f")
              .asInstanceOf[Obj => Unit => ?](ths)(()).asInstanceOf[Int]
    }

  val obj2: Obj = (m: String) =>
    m match {
      case "f"    => (ths: Obj) => (x: Unit) => 3
      case "sub1" => (ths: Obj) => (x: Int) => x - 1
      case _      => (ths: Obj) => obj1(m).asInstanceOf[Obj => ? => ?](ths)
    }

  def main(args: Array[String]): Unit =
    println(obj2("add1").asInstanceOf[Obj => Int => Int](obj2)(3))

}
