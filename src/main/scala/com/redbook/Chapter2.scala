package com.redbook

import scala.annotation.tailrec

object Exercise1 {
  // Write tail recursive fibonacci

  // @tailrec // should throw a compile error
  def fib: Int => Int = {
    case n if n < 1 =>
      throw new IllegalArgumentException(s"$n cannot be less than 1")
    case 1 => 0
    case 2 => 1
    case n => fib(n - 1) + fib(n - 2) // not tailrecursive
  }

  def tailrecfib(n: Int): Int = {
    @tailrec
    def aux(prev1: Int, prev2: Int, n: Int): Int =
      if (n == 0) prev2
      else aux(prev2, prev1 + prev2, n - 1)

    n match {
      case n if n < 1 =>
        throw new IllegalArgumentException(s"$n cannot be less than 1")
      case 1 => 0
      case 2 => 1
      case n => aux(0, 1, n - 2)
    }
  }

  def main(args: Array[String]): Unit = {
    (1 to 10).map { n =>
      val f = fib(n)
      println(s"$n: $f")
      assert(f == tailrecfib(n))
    }

  }
}

object Exercise2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as match {
      case as if as.length < 2 => true
      case Array(first, tail @ _*) =>
        tail
          .foldLeft((true, first)) { case ((acc, e1), e2) =>
            (ordered(e1, e2) && acc, e2)
          }
          ._1
    }
  }

  def main(args: Array[String]): Unit = {
    assert(isSorted[Int](Array(1, 2, 3, 4, 5), (a, b) => a <= b))
    assert(!isSorted[Int](Array(4, 2, 3, 4, 5), (a, b) => a <= b))
  }
}

object Exercise3 {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)
}

object Exercise4 {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)
}

object Exercise5 {
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
