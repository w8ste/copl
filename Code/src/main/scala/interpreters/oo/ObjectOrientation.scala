package interpreters.oo

object ObjectOrientation {

  sealed trait OOExpr
  case class Id(x: String)                                       extends OOExpr
  case class New(className: String, fields: List[OOExpr])        extends OOExpr
  case class GetField(o: OOExpr, field: String)                  extends OOExpr
  case class Call(o: OOExpr, method: String, args: List[OOExpr]) extends OOExpr

  type Value = Object
  case class Object(className: String, fieldValues: List[Value])

  type Env = Map[String, Value]

  case class Method(params: List[String], body: OOExpr)

  trait Common {

    type Class

    /** Returns the first occurrence of field in the path from the root to className in the inheritance tree */
    def lookupField(
        fieldName: String,
        obj: Object,
        classes: Map[String, Class]
    ): Option[Value]

    /** Returns the first method found in the path from className to the root in the inheritance tree */
    def lookupMethod(
        methodName: String,
        className: String,
        classes: Map[String, Class]
    ): Option[Method]

    def interp(e: OOExpr, env: Env, classes: Map[String, Class]): Value = e match {

      // this version of the ID case tries to lookup a field in the current object in case no entry in the environment could be found
      case Id(id) =>
        env.get(id) match
            case Some(value) => value
            case None        =>
              interp(GetField(Id("this"), id), env, classes)

      case New(className, args) =>
        if !classes.contains(className) then
            sys.error(s"Can not initialize unknown class $className")
        val argValues = args.map { expr => interp(expr, env, classes) }
        Object(className, argValues)

      case GetField(objExpr, fieldName) =>
        val receiver = interp(objExpr, env, classes)
        lookupField(fieldName, receiver, classes) match
            case Some(value) => value
            case None        => sys.error(s"unknown field $fieldName for ${receiver.className}")

      case Call(objExpr, methodName, args) =>
        val receiver = interp(objExpr, env, classes)
        val method   = lookupMethod(methodName, receiver.className, classes).getOrElse {
          sys.error(s"Unknown method $methodName for class ${receiver.className}")
        }
        val argVals     = args.map { arg => interp(arg, env, classes) }
        val argBindings = method.params zip argVals
        val newEnv      = argBindings.toMap + ("this" -> receiver)
        interp(method.body, newEnv, classes)

    }

  }

  object Basic extends Common {

    case class Class(
        fields: List[String],
        methods: Map[String, Method],
    )

    def lookupField(
        fieldName: String,
        obj: Object,
        classes: Map[String, Class]
    ): Option[Value] =
      classes.get(obj.className).flatMap { cls =>
        val fieldIndex = cls.fields.indexOf(fieldName)
        // `lift` on lists is like `get` on maps, returning `None` in case of being out of bounds
        // the name `lift` is because we “lift” this partial function (that may throw an exception)
        // into a total function that always returns a value (even if that value is None)
        obj.fieldValues.lift(fieldIndex)
      }

    def lookupMethod(
        methodName: String,
        className: String,
        classes: Map[String, Class]
    ): Option[Method] =
      classes.get(className).flatMap(cls => cls.methods.get(methodName))

  }

  object Inheritance extends Common {

    case class Class(
        superClass: String,
        fields: List[String],
        methods: Map[String, Method],
    )

    /* Not stack safe, but let’s assume we don’t have very large class hierarchies */
    def fieldsOf(className: String, classes: Map[String, Class]): List[String] =
      classes.get(className).toList.flatMap { cls =>
        fieldsOf(cls.superClass, classes) ++ cls.fields
      }

    def lookupField(
        fieldName: String,
        obj: Object,
        classes: Map[String, Class]
    ): Option[Value] =
        val index = fieldsOf(obj.className, classes).lastIndexOf(fieldName)
        obj.fieldValues.lift(index)

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

}
