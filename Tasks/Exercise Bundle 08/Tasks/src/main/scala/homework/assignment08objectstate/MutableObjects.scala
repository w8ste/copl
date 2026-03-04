package homework.assignment08objectstate

import interpreters.memory.RootedEnv
import interpreters.memory.stores.Store

object MutableObjects {

  sealed trait OOExpr
  case class New(className: String, fields: OOExpr*)                  extends OOExpr
  case class GetField(objExpr: OOExpr, fieldName: String)             extends OOExpr
  case class SetField(objExpr: OOExpr, fieldName: String, e: OOExpr)  extends OOExpr
  case class Call(objExpr: OOExpr, methodName: String, args: OOExpr*) extends OOExpr

  // common expressions not related to OO
  case class Id(name: String)                                   extends OOExpr
  case class Num(n: Int)                                        extends OOExpr
  case class Add(lhs: OOExpr, rhs: OOExpr)                      extends OOExpr
  case class Mult(lhs: OOExpr, rhs: OOExpr)                     extends OOExpr
  case class Let(name: String, namedExpr: OOExpr, body: OOExpr) extends OOExpr
  case class SetId(id: String, valueExpr: OOExpr)               extends OOExpr
  case class Seqn(exprs: OOExpr*)                               extends OOExpr {
    // This is a runtime check that is performed when the value is constructed.
    require(exprs.nonEmpty, "Seqn may not be empty")
  }

  case class Class(
      superClass: String,
      fields: Seq[String],
      methods: Map[String, Method]
  )

  case class Method(body: OOExpr, params: String*)

  type Location = Int
  type Env      = Map[String, Location]

  type Value = Object | Num
  case class Object(className: String, fieldValues: List[Location])

  /* In our interpreter, the stack of environments is only implicitly
   * available on the stack of the meta-language. To reify the call-
   * stack we need to make it explicit. We do so by constructing the
   * stack explicitly and passing it as parameter. The first element
   * of the stack is the current environment; the rest is only needed
   * for gc.
   */
  def interp(
      expr: OOExpr,
      stack: RootedEnv,
      store: Store[Value],
      classes: Map[String, Class]
  ): (Value, Store[Value]) = {

    def binOp(lhs: OOExpr, rhs: OOExpr, combine: (Int, Int) => Int): (Value, Store[Value]) =
        val (lv, s1) = interp(lhs, stack, store, classes)
        val (rv, s2) = interp(rhs, stack, s1, classes)
        (lv, rv) match {
          case (Num(m), Num(n)) => (Num(combine(m, n)), s2)
          case _                => sys.error(s"Can do arthmetic with numbers, but got $lhs and $rhs")
        }

    /** allocates a sequence of expressions */
    def allocateAll(exprs: Seq[OOExpr], store: Store[Value]): (List[Location], Store[Value]) = {
      val (argLocations, s3) = exprs.foldLeft((List.empty[Location], store)) { case ((values, s0), expr) =>
        val (v, s1)  = interp(expr, stack, s0, classes)
        val (vl, s2) = s1.malloc(v, stack.roots ++ values)
        (values :+ vl, s2)
      }
      (argLocations, s3)
    }

    expr match {

      case Num(n) => (Num(n), store)

      case Add(lhs, rhs)  => binOp(lhs, rhs, _ + _)
      case Mult(lhs, rhs) => binOp(lhs, rhs, _ * _)

      case Let(boundId, namedExpr, boundBody) =>
        val (namedVal, s1) = interp(namedExpr, stack, store, classes)
        val (newLoc, s2)   = s1.malloc(namedVal, stack.roots)
        val env            = stack + (boundId -> newLoc)
        interp(boundBody, env, s2, classes)

      // Note the * to match all varargs
      case Seqn(elems*) =>
        if elems.isEmpty then sys.error("cannot evaluate empty Seqn")
        elems.foldLeft((Num(-42): Value, store)) { case ((_, storeAcc), expr) =>
          val (valueRes, storeRes) = interp(expr, stack, storeAcc, classes)
          (valueRes, storeRes)
        }

      case Id(name) =>
        stack.get(name) match
            case Some(loc) =>
              (store.lookup(loc), store)
            case None =>
              interp(GetField(Id("this"), name), stack, store, classes)

      case SetId(id, valExpr) => ???

      case New(className, args*) => ???

      case GetField(objExpr, fieldName) => ???

      case SetField(objExpr, fieldName, e) => ???

      case Call(objExpr, methodName, args*) => ???

    }
  }

  /** Not stack safe, but let’s assume we don’t have very large class hierarchies */
  def fieldsOf(className: String, classes: Map[String, Class]): List[String] =
    classes.get(className).toList.flatMap { cls =>
      fieldsOf(cls.superClass, classes) ++ cls.fields
    }

  /** Returns the first occurrence of field in the path from the root to className in the inheritance tree */
  def lookupField(
      fieldName: String,
      obj: Object,
      classes: Map[String, Class]
  ): Option[Location] =
      val index = fieldsOf(obj.className, classes).lastIndexOf(fieldName)
      obj.fieldValues.lift(index)

  /** Returns the first method found in the path from className to the root in
    * the inheritance tree or None
    */
  def lookupMethod(
      methodName: String,
      className: String,
      classes: Map[String, Class]
  ): Option[Method] = className match {
    case "Object" => None
    case _        =>
      classes.get(className).flatMap { cls =>
        cls.methods.get(methodName)
          .orElse(lookupMethod(methodName, cls.superClass, classes))
      }

  }
}
