package com.redbook

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Chapter6 {
  type Rand[+A] = RNG => (A, RNG)
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Ex 1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, r) = rng.nextInt
    if (x == Integer.MIN_VALUE) {
      val (i, r2) = rng.nextInt
      (Math.abs(i), r2)
    } else (Math.abs(x), r)
  }

  // Ex 2
  def double(rng: RNG): (Double, RNG) = {
    val (x, r) = rng.nextInt
    (Math.abs(x.toDouble / Integer.MAX_VALUE), r)
  }

  // Ex 3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  // Ex 3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = nonNegativeInt(r1)
    ((d, i), r2)
  }

  // Ex 3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // Ex 4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List(), rng)
    else {
      val (n, r) = rng.nextInt
      val (l, r1) = ints(count - 1)(r)
      (n :: l, r1)
    }
  }

  // Ex 5
  def double2: Rand[Double] =
    map(_.nextInt)(x => Math.abs(x.toDouble / Integer.MAX_VALUE))

  // Ex 6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (x1, r1) = ra(rng)
      val (x2, r2) = rb(r1)
      (f(x1, x2), r2)
    }

  // Ex 7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldLeft((List[A](), rng)) { case ((l, r), f) =>
        val (x, r1) = f(r)
        (x :: l, r1)
      }
    }

  def randInts(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))

  // Ex 8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(a =>
    rng => {
      val mod = a % n
      if (a + (n - 1) - mod >= 0) (mod, rng) else nonNegativeLessThan(n)(rng)
    }
  )

  // Ex 9
  def altmap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a => rng => (f(a), rng) }

  def altdouble: Rand[Double] =
    altmap(_.nextInt)(x => Math.abs(x.toDouble / Integer.MAX_VALUE))

  def altmap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a => rng =>
      val (b, r2) = rb(rng)
      (f(a, b), r2)
    }
  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(1)

    def printSome[T](f: Rand[T]): Unit = {
      (0 to 5).foldRight(rng: RNG) { (_, r) =>
        val (x, r1) = f(r)
        println(x)
        r1
      }
      println()
    }

    printSome(nonNegativeInt)
    printSome(double)
    printSome(intDouble)
    printSome(doubleInt)
    printSome(double3)
    printSome(ints(3))
    printSome(double2)
    printSome(map2(nonNegativeInt, double2)((x, y) => (x, y)))
    printSome(randInts(3))
    printSome(nonNegativeLessThan(3))
    printSome(altdouble)
    printSome(altmap2(nonNegativeInt, double2)((x, y) => (x, y)))
  }
}

// Ex 10
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](run2: S => (B, S))(f: (A, B) => C): S => (C, S) = s => {
    val (a, s1) = run(s)
    val (b, s2) = run2(s1)
    (f(a, b), s2)
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      g(a).run(s1)
    })
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    State(s => {
      fs.foldLeft((List[A](), s)) { case ((l, s), f) =>
        val (x, s1) = f.run(s)
        State((s2: S) => (x :: l, s2)).run(s1)
      }
    })
  }

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object StateRand {
  type Rand[+A] = State[RNG, A]

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(1)

    def printSome[T](f: Rand[T]): Unit = {
      (0 to 5).foldRight(rng: RNG) { (_, r) =>
        val (x, r1) = f.run(r)
        println(x)
        r1
      }
      println()
    }

    def nonNegativeInt: Rand[Int] =
      State(r => {
        val (x, r1) = r.nextInt
        if (x == Integer.MIN_VALUE) {
          val (i, r2) = rng.nextInt
          (Math.abs(i), r2)
        } else (Math.abs(x), r1)
      })

    def double: Rand[Double] =
      for (i <- nonNegativeInt) yield Math.abs(i.toDouble / Integer.MAX_VALUE)

    def ints(count: Int): Rand[List[Int]] =
      State.sequence[RNG, Int](List.fill(3)(State(_.nextInt)))

    def doubleInt: Rand[(Double, Int)] =
      for {
        i <- nonNegativeInt
        d <- double
      } yield (d, i)

    def nonNegativeLessThan(n: Int): Rand[Int] =
      nonNegativeInt.flatMap(a => {
        val mod = a % n
        if (a + (n - 1) - mod >= 0) State.unit(mod)
        else nonNegativeLessThan(n)
      })

    printSome(double)
    printSome(ints(3))
    printSome(doubleInt)
    printSome(nonNegativeLessThan(3))
  }
}

// Ex 11
sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def step(input: Input): Machine =
    input match {
      case Coin if locked && candies > 0 => Machine(false, candies, coins + 1)
      case Turn if !locked               => Machine(true, candies - 1, coins)
      case _                             => this
    }
}

object CandyMachine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- State.sequence(inputs.map { i => State.modify[Machine](_.step(i)) })
      m <- State.get
    } yield (m.candies, m.coins)
  }

  def main(args: Array[String]): Unit = {
    val res =
      simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
        .run(Machine(true, 5, 10))
        ._1
    println(res)
  }
}
