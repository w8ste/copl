package exercises.ex05continuations

import scala.util.boundary

object ContinuationPassingStyleTransformation {

  def sum(n: Int): Int = {
    if n == 1 then
        1
    else
        n + sum(n - 1)
  }

  def insert(elem: Int, list: List[Int]): List[Int] = {
    list match {
      case x :: xs =>
        if elem < x then
            elem :: list
        else
            x :: insert(elem, xs)
      case Nil => List(elem)
    }
  }

  def insertionSort(list: List[Int]): List[Int] = {
    list match {
      case x :: xs => insert(x, insertionSort(xs))
      case Nil     => Nil
    }
  }

  def merge(left: List[Int], right: List[Int]): List[Int] =
    (left, right) match {
      case (x :: xs, y :: ys) =>
        if x < y then
            x :: merge(xs, right)
        else
            y :: merge(left, ys)
      case (Nil, _) => right
      case (_, Nil) => left
    }

  def mergeSort(list: List[Int]): List[Int] = {
    if list.size < 2 then
        list
    else {
      val (left, right) = list.splitAt(list.size / 2)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def quicksort(list: List[Int]): List[Int] = {
    if list.sizeIs <= 1 then list
    else
        val pivot             = list.head
        val (smaller, larger) = list.tail.partition(x => x <= pivot)
        quicksort(smaller) concat (pivot :: quicksort(larger))
  }

  def main(args: Array[String]): Unit = {
    /*
    
    println(boundary {
      sumCPS(10, boundary.break(_))
    })

    val testList = List(5, 3, 9, 0, 6, 1, 4, 2, 8, 7)
    println(boundary {
      insertionSortCPS(testList, boundary.break(_))
    })

    println(boundary {
      mergeSortCPS(testList, boundary.break(_))
    })

    println(boundary {
      quicksortCPS(testList, boundary.break(_))
    })

    val example = App(Fun("x", Add(Id("x"), Id("x"))), Sub(Num(2), Num(1)))

    // should print 8
    println(boundary {
      countFAECPS(example, boundary.break(_))
    })
    
     */
  }

  sealed trait FAE
  case class Num(n: Int)                       extends FAE
  case class Add(lhs: FAE, rhs: FAE)           extends FAE
  case class Sub(lhs: FAE, rhs: FAE)           extends FAE
  case class Id(name: String)                  extends FAE
  case class Fun(param: String, bodyExpr: FAE) extends FAE
  case class App(funExpr: FAE, argExpr: FAE)   extends FAE

  def countFAE(expr: FAE): Int =
    expr match {
      case Num(_)                => 1
      case Add(lhs, rhs)         => countFAE(lhs) + countFAE(rhs) + 1
      case Sub(lhs, rhs)         => countFAE(lhs) + countFAE(rhs) + 1
      case Id(_)                 => 1
      case Fun(_, bodyExpr)      => countFAE(bodyExpr) + 1
      case App(funExpr, argExpr) => countFAE(funExpr) + countFAE(argExpr) + 1
    }

}
