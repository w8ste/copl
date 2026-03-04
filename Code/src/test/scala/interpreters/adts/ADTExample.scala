object Task2 {

  import modularized.environmental.FullInterpreter.*

  // Define Bool data type
  val trueE: Data  = Data("true")
  val falseE: Data = Data("false")

  // Define natural numbers
  val zero: Data = Data("zero")
  val one: Data  = Data("succ", List(zero))
  val two: Data  = Data("succ", List(one))

  // Define lists
  val nil: Data         = Data("nil")
  val exampleList: Data = Data("cons", List(two, Data("cons", List(one, Data("cons", List(zero, nil)))))) // (2, 1, 0)

  // Define length of a list
  val lengthExpr: LetRec =
    LetRec(
      "length",
      TypedFun(
        "list",
        TUnspecified,
        Match(
          "list",
          List(
            ("nil", Nil, 0),
            ("cons", List("x", "xs"), Add(1, App("length", "xs")))
          )
        )
      ),
      "length"
    )

  // Define a function that filters out zeros
  val isNotZero: TypedFun = TypedFun(
    "n",
    TUnspecified,
    Match(
      "n",
      List(
        ("zero", List(), falseE),
        ("succ", List("prev"), trueE)
      )
    )
  )

  // Define function filterNatList with f as fixed point
  val filterProgram: LetRec =
    LetRec(
      "filter",
      TypedFun(
        "predicate",
        TUnspecified,
        TypedFun(
          "list",
          TUnspecified,
          Let(
            "filterRec",
            TypedFun("x", TUnspecified, App(App("filter", "predicate"), "x")),
            Match(
              "list",
              List(
                ("nil", List(), nil),
                (
                  "cons",
                  List("x", "xs"),
                  Match(
                    App("predicate", "x"),
                    List(
                      ("false", List(), App("filterRec", "xs")),
                      ("true", List(), Data("cons", List("x", App("filterRec", "xs"))))
                    )
                  )
                )
              )
            )
          )
        )
      ),
      "filter"
    )

  def main(args: Array[String]): Unit = {
    println(interp(App(lengthExpr, exampleList)))
    println(interp(App(App(filterProgram, isNotZero), exampleList)))
  }

  object Syntax {
    extension (param: String) infix def ~>:(body: Expr): TypedFun = TypedFun(param, TUnspecified, body)

    extension (constructor: String)
        def into(names: String*)(body: Expr): (String, List[String], Expr) = (constructor, names.toList, body)

    extension (expr: Expr)
        def destructure(cases: (String, List[String], Expr)*): Match = Match(expr, cases.toList)

    extension (expr: Expr) def apply(arg: Expr): App = App(expr, arg)

    // Define function filterNatList with f as fixed point
    val filterProgram2: LetRec =
      LetRec(
        "filter",
        "predicate" ~>: "list" ~>: Let(
          "filterRec",
          "x" ~>: "filter" ("predicate")("x"),
          "list".destructure(
            "nil".into():
                nil
            ,
            "cons".into("x", "xs"):
                "predicate" ("x").destructure(
                  "false".into():
                      "filterRec" ("xs")
                  ,
                  "true".into():
                      Data("cons", List("x", "filterRec" ("xs")))
                )
          )
        ),
        "filter"
      )

  }

}
