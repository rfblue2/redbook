package com.redbook

object Chapter14 {
  sealed trait ST[S, A] {
    self =>
    protected def run(s: S): (A, S)
    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }

    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
      def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }

  object ST {
    def apply[S, A](a: => A) = {
      lazy val memo = a
      new ST[S, A] {
        def run(s: S) = (memo, s)
      }
    }

    def runST[A](st: RunnableST[A]): A =
      st.apply[Unit].run(())._1
  }

  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }

  sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
    protected def value: Array[A]

    def size: ST[S, Int] = ST(value.size)

    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        value(i) = a
        ((), s)
      }
    }

    def read(i: Int): ST[S, A] = ST(value(i))

    def freeze: ST[S, List[A]] = ST(value.toList)

    // Ex1
    def fill(xs: Map[Int, A]): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S): (Unit, S) = {
        xs.foldRight(ST[S, Unit](())) { case ((k, v), st) =>
          st.flatMap(_ => write(k, v))
        }.run(s)
      }
    }

    def swap(i: Int, j: Int): ST[S, Unit] = for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()
  }

  object STArray {
    def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
      ST(new STArray[S, A] {
        lazy val value = xs.toArray
      })
  }

  // Ex2
  def partition[S](
      arr: STArray[S, Int],
      n: Int,
      r: Int,
      pivot: Int
  ): ST[S, Int] = for {
    pivotVal <- arr.read(pivot)
    _ <- arr.swap(pivot, r)
    jj <- (n until r).foldRight(ST[S, Int](n)) { case (i, st) =>
      st.flatMap(j =>
        for {
          arri <- arr.read(i)
          j1 <-
            if (arri < pivotVal) arr.swap(i, j).map(_ => j + 1)
            else ST[S, Int](j)
        } yield j1
      )
    }
  } yield jj

  def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = if (n < r)
    for {
      pi <- partition(a, n, r, n + (n - r) / 2)
      _ <- qs(a, n, pi - 1)
      _ <- qs(a, pi + 1, r)
    } yield ()
  else ST[S, Unit](())

  def quicksort(xs: List[Int]): List[Int] = {
    if (xs.isEmpty) xs
    else
      ST.runST(new RunnableST[List[Int]] {
        def apply[S]: ST[S, List[Int]] = for {
          arr <- STArray.fromList(xs)
          size <- arr.size
          _ <- qs(arr, 0, size - 1)
          sorted <- arr.freeze
        } yield sorted
      })
  }

  // Ex3
  sealed abstract class STMap[S, K, V] {
    protected def value: scala.collection.mutable.HashMap[K, V]
    def keys: ST[S, List[K]] = ST(value.keys.toList)

    def put(k: K, v: V): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        value(k) = v
        ((), s)
      }
    }

    def get(k: K): ST[S, V] = ST(value(k))

    def freeze: ST[S, Map[K, V]] = ST(value.toMap)
  }

  object STMap {
    def fromMap[S, K, V](xs: Map[K, V]): ST[S, STMap[S, K, V]] =
      ST(new STMap[S, K, V] {
        lazy val value = scala.collection.mutable.HashMap.from(xs)
      })
  }
}
