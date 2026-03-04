package homework.assignmentXXadts

import modularized.environmental.FullInterpreter.*

import scala.language.implicitConversions

object Task2 {

  val insert: TypedFun =
    TypedFun("env", TUnspecified, TypedFun("symvalpair", TUnspecified, Data("Cons", List("symvalpair", "env"))))

  val lookup: LetRec = LetRec(
    "lookup",
    TypedFun(
      "env",
      TUnspecified,
      TypedFun(
        "sym",
        TUnspecified,
        Match(
          "env",
          List(
            (
              "Cons",
              List("head", "tail"),
              Match(
                "head",
                List(
                  (
                    "Pair",
                    List("envsym", "envval"),
                    If(Eq("envsym", "sym"), "envval", App(App("lookup", "tail"), "sym"))
                  )
                )
              )
            )
          )
        )
      )
    ),
    "lookup"
  )

  // TODO Your implementation here
  val flaeInterp = LetRec(
    "interp",
    TypedFun(
      "expr",
      TUnspecified,
      TypedFun(
        "env",
        TUnspecified,
        Match(
          "expr",
          List(
            ("Num", List("n"), ???),
            ("Add", List("lhs", "rhs"), ???),
            ("Sub", List("lhs", "rhs"), ???),
            ("Mult", List("lhs", "rhs"), ???),
            ("Let", List("boundId", "namedExpr", "body"), ???),
            ("Id", List("name"), ???),
            ("Fun", List("arg", "body"), ???),
            ("App", List("funExpr", "argExpr"), ???)
          )
        )
      )
    ),
    "interp"
  )
}
