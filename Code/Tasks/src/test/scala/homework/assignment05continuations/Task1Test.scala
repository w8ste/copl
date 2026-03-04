package homework.assignment05continuations

import homework.assignment05continuations.Task1.*

class Task1Test extends munit.FunSuite {

  case class EndOfWorld(value: Any) extends Throwable

  test("sumList") {
    assertEquals(
      {
        try sumListCPS(List(1, 2, 3, 4, 5), ret => throw EndOfWorld(ret))
        catch {
          case EndOfWorld(ret) => ret
        }
      },
      15
    )
  }

  test("fibn") {
    Range(1, 10).foreach { n =>
      assertEquals(
        {
          try fibCPS(n, ret => throw EndOfWorld(ret))
          catch {
            case EndOfWorld(ret) => ret
          }
        },
        fib(n)
      )
    }
  }

  test("fib2") {
    assertEquals(
      {
        try fibCPS(2, ret => throw EndOfWorld(ret))
        catch {
          case EndOfWorld(ret) => ret
        }
      },
      1
    )
  }

  test("fib10") {
    assertEquals(
      {
        try fibCPS(10, ret => throw EndOfWorld(ret))
        catch {
          case EndOfWorld(ret) => ret
        }
      },
      55
    )
  }

  test("fibAfterSum1") {
    assertEquals(
      {
        try fibAfterSumListCPS(List.empty, ret => throw EndOfWorld(ret))
        catch {
          case EndOfWorld(ret) => ret
        }
      },
      0
    )
  }

  test("fibAfterSum2") {
    assertEquals(
      {
        try fibAfterSumListCPS(List(1, 2, 3), ret => throw EndOfWorld(ret))
        catch {
          case EndOfWorld(ret) => ret
        }
      },
      8
    )
  }

  test("map") {
    assertEquals(
      {
        try mapCPS[Int, Boolean]((x, k) => k(x % 2 == 0), List(1, 2, 3, 4), ret => throw EndOfWorld(ret))
        catch {
          case EndOfWorld(ret) => ret
        }
      },
      List(1, 2, 3, 4).map(x => x % 2 == 0)
    )
  }

}
