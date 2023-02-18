package com.redbook

object Chapter5 {
  sealed trait Stream[+A] {
    import Stream._

    // ex1
    def toList: List[A] = this match {
      case Empty      => List()
      case Cons(h, t) => h() :: t().toList
    }

    // ex2
    def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (n == 0) Empty
        else Cons(h, () => t().take(n - 1))
    }

    def drop(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (n == 0) Cons(h, t)
        else t().drop(n - 1)
    }

    // ex3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (p(h())) Cons(h, () => t().takeWhile(p))
        else t().takeWhile(p)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case Empty      => z
      }

    // ex4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    // ex5
    def takeWhileAlt(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

    // ex6
    def headOption: Option[A] =
      foldRight(None: Option[A])((a, b) => Some(a))

    // ex7
    def map[B](f: A => B): Stream[B] =
      foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

    def append[B >: A](x: => B): Stream[B] =
      foldRight((s: Stream[B]) => cons(x, s))((a, b) => s => cons(a, b(s)))(
        Empty
      )

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Empty: Stream[B])((a, b) =>
        f(a).foldRight(b)((a2, b2) => cons(a2, b2))
      )

    // ex13
    def mapUf[B](f: A => B): Stream[B] =
      unfold(this) {
        case Empty      => None
        case Cons(h, t) => Some(f(h()), t())
      }

    def takeUf(n: Int): Stream[A] =
      unfold((n, this)) { case (n, ss) =>
        if (n == 0) None
        else {
          ss match {
            case Empty      => None
            case Cons(h, t) => Some(h(), (n - 1, t()))
          }
        }
      }

    def takeWhileUf(p: A => Boolean): Stream[A] =
      unfold(this) {
        case Empty      => None
        case Cons(h, t) => if (p(h())) Some(h(), t()) else None
      }

    def zipWith[B >: A, C](ss2: Stream[B])(f: (B, B) => C): Stream[C] = {
      unfold((this, ss2)) {
        case (_, Empty) | (Empty, _)      => None
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      }
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((this, s2)) {
        case (Empty, Empty)      => None
        case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
        case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Some((Some(h1()), Some(h2())), (t1(), t2()))
      }

    // ex14
    def startsWith[A](s: Stream[A]): Boolean =
      zipWith(s)(_ == _).foldRight(true)(_ && _)

    // ex15
    def tailsUf: Stream[Stream[A]] =
      unfold(Some(this): Option[Stream[A]]) {
        case None             => None
        case Some(Empty)      => Some(Empty, None)
        case Some(Cons(h, t)) => Some(Cons(h, t), Some(t()))
      }

    // ex16
    // this uses unfold but doesn't reuse intermediate results :(
    def scanRight[B >: A, C](z: C)(f: (B, C) => C): Stream[C] = {
      unfold(Some(this): Option[Stream[B]]) {
        case None        => None
        case Some(Empty) => Some(z, None)
        case Some(Cons(h, t)) =>
          Some((Cons(h, t).foldRight(z)((a, b) => f(a, b)), Some(t())))
      }
    }
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    // ex8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // ex9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    // ex10
    def fibs: Stream[Int] = {
      def fib(a: Int, b: Int): Stream[Int] = cons(a, fib(b, a + b))
      fib(0, 1)
    }

    // ex11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z).fold(Empty: Stream[A]) { case (a, s) => cons(a, unfold(s)(f)) }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).toList)
    println(Stream(1, 2, 3, 4).take(2).toList)
    println(Stream(1, 2, 3, 4).drop(2).toList)
    println(Stream(1, 2, 3, 4).takeWhile(_ < 4).toList)
    println(
      Stream
        .cons(
          { println("1"); 1 },
          Stream.cons(
            { println("2"); 2 },
            Stream.cons(
              { println("3"); 3 },
              Stream.cons(
                { println("4"); 4 },
                Empty
              )
            )
          )
        )
        .forAll(_ < 3)
    ) // should not print 4

    println(Stream(1, 2, 3, 4).takeWhileAlt(_ < 4).toList)
    println(Stream(1, 2, 3, 4).headOption)
    println(Empty.headOption)
    println(Stream(1, 2, 3, 4).map(_ * 2).toList)
    println(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList)
    println(Stream(1, 2, 3, 4).append(5).toList)
    println(Stream(1, 2, 3, 4).flatMap { x => Stream(1, x) }.toList)
    println(Stream.constant(1).take(3).toList)
    println(Stream.from(1).take(3).toList)
    println(Stream.fibs.take(5).toList)
    println(Stream.unfold(1)(s => Some((s, s + 1))).take(3).toList)
    println(
      Stream
        .unfold(1)(s => if (s == 3) None else Some((s, s + 1)))
        .toList
    )

    // ex12
    def fibs: Stream[Int] =
      Stream.unfold((0, 1)) { case (prev, next) =>
        Some((prev), (next, prev + next))
      }
    def from(n: Int): Stream[Int] = Stream.unfold(n)(s => Some(s, s + 1))
    def constant(n: Int): Stream[Int] = Stream.unfold(n)(s => Some(s, s))
    def ones: Stream[Int] = Stream.unfold(1)(s => Some(1, 1))

    println(fibs.take(5).toList)
    println(from(3).take(3).toList)
    println(constant(3).take(3).toList)
    println(ones.take(3).toList)

    println(ones.mapUf(_ + 1).takeUf(3).toList)
    println(from(1).takeWhileUf(_ < 4).toList)
    println(constant(1).zipWith(from(2))(_ + _).take(3).toList)
    println(from(1).zipAll(constant(2).take(3)).take(4).toList)

    println(Stream(1, 2, 3).startsWith(Stream(1, 2)))
    println(Stream(1, 2, 3).startsWith(Stream(1, 1)))

    println(Stream(1, 2, 3).tailsUf.map(_.toList).toList)

    println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
    println(
      Stream(1, 2, 3)
        .scanRight(Empty: Stream[Int])((a, s) => Stream.cons(a, s))
        .map(_.toList)
        .toList
    )
  }
}
