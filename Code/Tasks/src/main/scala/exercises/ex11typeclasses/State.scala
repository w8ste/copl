package exercises.ex11typeclasses

import modularized.stateful.{Sequential, Step, given}

object StateExample {

  type Record = Int
  type DB     = Set[Record]

  def addRecord(record: Record): Step[DB, Boolean] =
    if record < 0
    then Sequential.ret(false)
    else Step(db => (true, db + record))

  def delRecord(record: Record): Step[DB, Boolean] = Step: db =>
      if db.contains(record)
      then (true, db - record)
      else (false, db)

  def runTransaction(db: DB)(tx: Step[DB, Boolean]): DB =
      val (success, newDB) = tx.run(db)
      if success
      then newDB
      else db

  val exampleUpdate: Step[DB, Boolean] =
    for
        b1 <- addRecord(1)
        b2 <- addRecord(2)
        b3 <- delRecord(3)
    yield b1 && b2 && b3

  // So far, we did not really make use of state being a monad
  // So here is one of the typical abstract functions you can define on a Monad:
  // If you have a list of monads `input` then you can compose all of them sequentially,
  // to produce a single monad that contains a list of the results (the values that the monad passes to the parameter of andThen/flatMap)
  def sequence[A, M[_]: Sequential](input: List[M[A]]): M[List[A]] =
    input match
        case Nil          => Sequential.ret(Nil)
        case head :: tail =>
          for
              h <- head           // “run” the head monad
              t <- sequence(tail) // “run” the remaining monads
          yield h :: t

  // The sequence example produces the same result as the other example, but using the sequence combinator.
  val sequenceExample: Step[DB, Boolean] =
    sequence(
      List(
        addRecord(1),
        addRecord(2),
        delRecord(3),
      )
    ).andThen: booleans =>
        Sequential.ret(booleans.reduce(_ && _))

  def main(args: Array[String]): Unit = {

    val db1 = Set(3, 4, 5)

    println(exampleUpdate.run(db1))

    val db2 = Set(4, 5)

    // If we just “execute” the example update, it will “fail” (return false) because db2 did not contain the record 3, so 3 could not be removed.
    // However, the two additions before will still be part of the resulting DB.
    println(exampleUpdate.run(db2))

    // If that is NOT what we want, and we want to not have updates take effect when the overall result is false,
    // we can easily extract that logic into a helper function that runs the update “transactionally”.
    println(runTransaction(db2)(exampleUpdate))

    println(sequenceExample.run(db1))
  }
}
