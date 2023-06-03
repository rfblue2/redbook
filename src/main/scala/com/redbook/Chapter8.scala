package com.redbook

import java.util.concurrent.Executors

object Chapter8Ex3 {
  // Ex 3
  trait Prop {
    def check: Boolean

    def &&(p: Prop): Prop = {
      new Prop {
        override def check: Boolean = check && p.check
      }
    }
  }
}

object Chapter8 {
  import Par._

  type Rand[A] = State[RNG, A]
  type MaxSize = Int

  import Prop._
  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    // Ex 9
    def &&(p: Prop): Prop = Prop { case (maxSize, cases, rng) =>
      run(maxSize, cases, rng) match {
        case Passed | Proved => p.run(maxSize, cases, rng)
        case f: Falsified    => f
      }
    }

    def ||(p: Prop): Prop = Prop { case (maxSize, cases, rng) =>
      run(maxSize, cases, rng) match {
        case Passed       => Passed
        case Proved       => Proved
        case f: Falsified => p.run(maxSize, cases, rng)
      }
    }
  }
  object Prop {
    type TestCases = Int
    type SuccessCount = Int
    type FailedCase = String

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      def isFalsified = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount)
        extends Result {
      def isFalsified = true
    }

    case object Proved extends Result {
      override def isFalsified: Boolean = false
    }

    def run(
        p: Prop,
        maxSize: Int = 100,
        testCases: Int = 100,
        rng: RNG = SimpleRNG(System.currentTimeMillis())
    ): Unit = {
      p.run(maxSize, testCases, rng) match {
        case Passed => println(s" + OK, passed $testCases tests.")
        case Proved => println("+ OK, proved property")
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
      }
    }

    def check(p: => Boolean): Prop = Prop { (_, _, _) =>
      if (p) Proved else Falsified("()", 0)
    }
  }

  case class Gen[A](sample: Rand[A]) {
    // Ex 6
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap { i =>
        Gen(Gen.sequence(List.fill(i) { sample }))
      }

    def map[B](f: A => B): Gen[B] =
      flatMap(a => Gen.unit(f(a)))

    // Ex 10
    def unsized: SGen[A] = SGen(_ => this)

    def map2[B, C](g2: Gen[B])(f: (A, B) => C): Gen[C] =
      Gen {
        for {
          a <- sample
          b <- g2.sample
        } yield f(a, b)
      }

    def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))
  }

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  object Gen {
    // Ex 4
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(intBetween(start, stopExclusive))

    private def intBetween(a: Int, b: Int): Rand[Int] = {
      int.flatMap { n =>
        val mod = n % (b - a)
        if (n + ((b - a) - 1) - mod >= 0) State.unit(mod + a)
        else intBetween(a, b)
      }
    }

    def int: Rand[Int] = State[RNG, Int](_.nextInt)

    def stringN(n: Int): Gen[String] =
      listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

    val string: SGen[String] = SGen(stringN)

    private def bool: Rand[Boolean] =
      for { i <- int } yield i % 2 == 0

    // Ex 5
    def unit[A](a: => A): Gen[A] =
      Gen(State.unit(a))

    def boolean: Gen[Boolean] = Gen(bool)

    private def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      //      def go(
      //          s: RNG,
      //          actions: List[State[RNG, A]],
      //          acc: List[A]
      //      ): (List[A], RNG) =
      //        actions match {
      //          case Nil => (acc.reverse, s)
      //          case h :: t =>
      //            h.run(s) match {
      //              case (a, s2) => go(s2, t, a :: acc)
      //            }
      //        }
      //
      //      State((s: RNG) => go(s, fs, List()))
      //    }
      for { rng <- State.get } yield {
        fs
          .foldLeft((List[A](), rng)) { case ((l, r), f) =>
            val (x, r1) = f.run(r)
            (x :: l, r1)
          }
      }._1
    }

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(sequence(List.fill(n)(g.sample)))

    // Ex 7
    def union[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] =
      for {
        a <- gen1
        b <- gen2
        c <- Gen(for (i <- int) yield if (i % 2 == 0) a else b)
      } yield c

    // Ex 8
    def double: Rand[Double] =
      for (i <- int) yield Math.abs(i.toDouble / Integer.MAX_VALUE)

    def weighted[A](gen1: (Gen[A], Double), gen2: (Gen[A], Double)): Gen[A] =
      for {
        a <- gen1._1
        b <- gen2._1
        c <- Gen(
          for (d <- double)
            yield if (d < (gen1._2 / (gen1._2 + gen2._2))) a else b
        )
      } yield c

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
      randomStream(as)(rng)
        .zip(Stream.from(0))
        .take(n)
        .map { case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    object ** {
      def unapply[A, B](p: (A, B)) = Some(p)
    }
  }

  case class SGen[A](forSize: Int => Gen[A]) {
    // Ex 11
//    def flatMap[B](f: A => SGen[B]): SGen[B] =
//      SGen(n => forSize(n).map(f).)

    def **[B](s2: SGen[B]): SGen[(A, B)] =
      SGen(n => apply(n) ** s2(n))

    def apply(n: Int): Gen[A] = forSize(n)
  }

  object SGen {
    def unit[A](a: => A): SGen[A] =
      Gen.unit(a).unsized

    // Ex 12
    def listOf[A](g: Gen[A]): SGen[List[A]] =
      SGen(n => Gen.listOfN(n, g))

    // Ex 13
    def listOf1[A](g: Gen[A]): SGen[List[A]] =
      SGen(n => Gen.listOfN(n + 1, g))

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      forAll(g.forSize)(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] =
          Stream.from(0).take((n min max) + 1).map(i => Gen.forAll(g(i))(f))
        val prop: Prop =
          props
            .map(p =>
              Prop { (max, _, rng) =>
                p.run(max, casesPerSize, rng)
              }
            )
            .toList
            .reduce(_ && _)
        prop.run(max, n, rng)
    }
  }

  val S = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    Gen.unit(Executors.newCachedThreadPool) -> 0.25
  )

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    Gen.forAll(S ** g) { case s ** a => f(a)(s).get }

  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)

    val maxProp = SGen.forAll(SGen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Prop.run(maxProp)

    // Ex 14
    val listProp = SGen.forAll(SGen.listOf(smallInt)) { ns =>
      val sorted = ns.sorted
      sorted.zip(sorted.drop(1)).forall { case (n1, n2) =>
        n1 <= n2
      }
    }
    Prop.run(listProp)

    val pint = Gen.choose(0, 10) map (Par.unit(_))
    val p5 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
    Prop.run(p5)

    // Ex 16
    val pnested = Gen.choose(0, 10) map { n =>
      Par.sequence(List.fill(n)(Par.unit(n)))
    }
    val p6 = forAllPar(pnested)(n => equal(Par.map(n)(y => y), n))
    Prop.run(p6)

    // Ex 17
    val p7 = forAllPar(pint)(n => equal(Par.fork(n), n))
    Prop.run(p7)

    // TODO not closing executors causes program to hang

    // Ex 18
    // s.takeWhile(f).dropWhile(f) == s.reverse.takeWhile(f).dropWhile(f)
  }
}
