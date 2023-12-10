package com.redbook

object Chapter11 {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
    def map[A, B](ma: F[A])(f: A => B): F[B] =
      flatMap(ma)(a => unit(f(a)))
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    // Ex3
    def sequence[A](lma: List[F[A]]): F[List[A]] =
      lma.foldRight(unit(List[A]()): F[List[A]]) { (next, acc) =>
        flatMap(acc) { l => map(next) { _ :: l } }
      }

    // Ex3
    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
      la.foldRight(unit(List[B]()): F[List[B]]) { (next, acc) =>
        flatMap(acc) { l => map(f(next))(_ :: l) }
      }
    }

    // Ex4
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      if (n <= 0) unit(List())
      else map2(ma, replicateM(n - 1, ma))(_ :: _)

    // Ex5
    // replicate M will generate list of n lists
    // option of list of n items
    // in general, a container containing a list of n things

    // Ex6
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
      ms.foldRight(unit(List[A]): F[List[A]]) { (next, acc) =>
        map2(f(next), acc) { (b, l) => if (b) next :: l else l }
      }
    }

    // Ex7
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)

    // Ex8
    def flatMapByCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
      compose((_: Unit) => ma, f)(())

    // Ex9
    /*
     * compose(compose(f, g), h) == compose(f, compose(g, h))
     * a => flatMap(compose(f, g)(a))(h) == a => flatMap(f(a))(compose(g)(h))
     * a => flatMap(a => flatMap(f(a))(g)) == a => flatMap(f(a))(a => flatMap(g(a)(h)))
     */

    // Ex10
    /*
     * compose(f, unit) == f <=> compose(unit, f) == f
     * a => flatMap(f(a))(unit) == f <=> a => flatMap(unit(a))(f) == f
     * let x = f(a), y = a
     * apply a
     * flatMap(x)(unit) == x <=> flatMap(unit(y))(f) == f(y)
     */

    // Ex11
    /*
     * Option Monad:
     *
     * val optionMonad = new Monad[Option] {
     *   override def unit[A](a: => A): Option[A] = Some(a)
     *
     *   override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
     *     ma.flatMap(f)
     * }
     *
     * flatMap(x)(a => Some(a)) == x
     * if x == None:
     * x.flatMap(a => Some(a)) == x <=> None == None
     * if x == Some(a)
     * x.flatMap(a => Some(a)) == x <=> Some(a).flatMap(a => Some(a)) == Some(a)
     */

    // Ex12
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(a => a)

    // Ex13
    // def map[A, B](ma: F[A])(f: A => B): F[B] =
    def flatMapByJoin[A, B](ma: F[A])(f: A => F[B]): F[B] =
      join(map(ma)(f))

    def composeByJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => join(map(f(a))(g))

  }

  object Monad {
    import Par._
    import Chapter8._
    import Chapter9._

    val genMonad = new Monad[Gen] {
      override def unit[A](a: => A): Gen[A] = Gen.unit(a)

      override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
        ma.flatMap(f)
    }

    // Ex1
    val parMonad = new Monad[Par] {
      override def unit[A](a: => A): Par[A] = Par.unit(a)

      override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
        Par.flatMap(ma)(f)
    }

    def parserMonad[Parser[+_]](p: Parsers[Parser]) = new Monad[Parser] {
      override def unit[A](a: => A): Parser[A] = p.succeed(a)

      override def flatMap[A, B](ma: Parser[A])(f: A => Parser[B]): Parser[B] =
        p.flatMap(ma)(f)
    }

    val optionMonad = new Monad[Option] {
      override def unit[A](a: => A): Option[A] = Some(a)

      override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
        ma.flatMap(f)
    }

    val listMonad = new Monad[List] {
      override def unit[A](a: => A): List[A] = List(a)

      override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
        ma.flatMap(f)
    }

    // Ex2
//    import Chapter6._
//    def stateMonad[S, +A] = new Monad[State[S, A]]

    // Ex17
    val identityMonad = new Monad[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)
      override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] =
        ma.flatMap(f)
    }

    def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
      override def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](ma: State[S, A])(
          f: A => State[S, B]
      ): State[S, B] = ma.flatMap(f)
    }

    // Ex18
    // replicateM feeds states into each other M times
    // map2 feeds one state into the other
    // sequence same as replicateM but with a given list of state transitions, saves result in a list

    // Ex19 - get/set = unit and set(s)/get = s
  }

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  // Ex20
  // it's a function that applies run of type R to yield type A
  case class Reader[R, A](run: R => A)

  object Reader {
    def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
      override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

      // Yields a function that applies f on result of ma
      override def flatMap[A, B](ma: Reader[R, A])(
          f: A => Reader[R, B]
      ): Reader[R, B] =
        Reader(r => f(ma.run(r)).run(r))
    }

    // replicateM -> applies same function M times
    // sequence -> applies a sequence of functions and keeps result in a list
    // join -> applies a function twice
  }
}
