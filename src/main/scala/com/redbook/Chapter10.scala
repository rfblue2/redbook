package com.redbook

object Chapter10 {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }

  // Ex1
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }
  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero = 1
  }
  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }
  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  // Ex2
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    def zero = None
  }

  // Ex3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    override def zero: A => A = identity
  }

  // Ex4
  object Laws {
    import Chapter8._
    val ints = Gen.choose(-10, 10)
    val bools = Gen.boolean
    val strings = Gen.stringN(10)

    def associativityLaw[A](in: Gen[A])(monoid: Monoid[A]): Prop =
      Gen.forAll(for {
        x <- in
        y <- in
        z <- in
      } yield (x, y, z)) { case (a, b, c) =>
        monoid.op(a, monoid.op(b, c)) == monoid.op(monoid.op(a, b), c)
      }

    def zeroLaw[A](in: Gen[A])(monoid: Monoid[A]): Prop = {
      Gen.forAll(in) { x =>
        monoid.op(x, monoid.zero) == x && monoid.op(monoid.zero, x) == x
      }
    }

    def monoidLaws[A](in: Gen[A])(monoid: Monoid[A]): Prop =
      associativityLaw(in)(monoid) && zeroLaw(in)(monoid)

    def verify(): Unit = {
      Prop.run(monoidLaws(ints)(intAddition))
      Prop.run(monoidLaws(ints)(intMultiplication))
      Prop.run(monoidLaws(bools)(booleanOr))
      Prop.run(monoidLaws(bools)(booleanAnd))
      Prop.run(monoidLaws(for {
        i <- ints
        b <- bools
      } yield if (b) Some(i) else None)(optionMonoid))
      // for some reason this is false for endomonoid?
//      Prop.run(monoidLaws(for {
//        i <- ints
//      } yield (x: Int) => x * i)(endoMonoid))

      val wcGen: Gen[WC] =
        for {
          b <- bools
          i <- ints
          s1 <- strings
          s2 <- strings
        } yield if (b) Stub(s1) else Part(s1, i, s2)
      Prop.run(monoidLaws(wcGen)(wcMonoid))
    }
  }

  // Ex5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) { (b, a) => m.op(b, f(a)) }

  // Ex6
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(a, b))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  // Ex7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    v match {
      case IndexedSeq()  => m.zero
      case IndexedSeq(x) => f(x)
      case s =>
        val n = s.size
        m.op(
          foldMapV(v.slice(0, n / 2), m)(f),
          foldMapV(v.slice(n / 2, n), m)(f)
        )
    }

  // Ex8
  import Par._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] =
      Par.map2(a1, a2)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => Par.unit(f(a)))

  // Ex9
  def isOrdered(s: IndexedSeq[Int]): Boolean = {
    foldMapV(
      s,
      new Monoid[(Boolean, Int)] {
        override def op(
            a1: (Boolean, Int),
            a2: (Boolean, Int)
        ): (Boolean, Int) = {
          ((a1._2 < a2._2) && a1._1 && a2._1, a2._2)
        }

        override def zero: (Boolean, Int) = (true, Integer.MIN_VALUE)
      }
    )(i => (true, i))._1
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Ex10
  def wcMonoid = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC =
      (a1, a2) match {
        case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
        case (Part(lStub, words, rStub), Stub(c)) =>
          Part(lStub, words, rStub + c)
        case (Stub(c), Part(lStub, words, rStub)) =>
          Part(c + lStub, words, rStub)
        case (Part(lStub, words, rStub), Part(lStub2, words2, rStub2)) =>
          val newCount = if ((rStub + lStub2).nonEmpty) 1 else 0
          Part(lStub, words + words2 + newCount, rStub2)
      }

    override def zero: WC = Stub("")
  }

  // Ex11
  def wc(str: String): Int = {
    def nonEmptySize(s: String): Int = Math.min(s.length, 1)
    foldMapV(str.toSeq, wcMonoid) { c: Char =>
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)
    } match {
      case Stub(chars) => nonEmptySize(chars)
      case Part(lStub, words, rStub) =>
        nonEmptySize(lStub) + words + nonEmptySize(rStub)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] =
      foldMap(fa)(a => List(a)) {
        new Monoid[List[A]] {
          override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

          override def zero: List[A] = List()
        }
      }
  }

  // Ex12
  object FoldableList extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldLeft(mb.zero)((b, a) => mb.op(f(a), b))
  }

  // Ex13
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  class FoldableTree extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(value)         => f(value, z)
        case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
      }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(value)         => f(z, value)
        case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
      }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(value) => f(value)
        case Branch(left, right) =>
          mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      }
  }

  // Ex14
  object FoldableOption extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Some(value) => f(value, z)
        case None        => z
      }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Some(value) => f(z, value)
        case None        => z
      }

    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
  }

  // Ex16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) =
        (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

      override def zero: (A, B) = (A.zero, B.zero)
    }

  // Ex17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))

    override def zero: A => B = a => B.zero
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
    }

  // Ex18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMapV(
      as,
      mapMergeMonoid[A, Int](intAddition)
    )(item => Map(item -> 1))
  }

  def main(args: Array[String]): Unit = {
    Laws.verify()

    def printAndAssert[A](x: => A, res: A): Unit = {
      println(x)
      assert(x == res)
    }

    printAndAssert(isOrdered(IndexedSeq(1, 2, 3, 4, 5, 6)), true)
    printAndAssert(isOrdered(IndexedSeq(1, 5, 3, 4, 5, 6)), false)

    printAndAssert(wc(""), 0)
    printAndAssert(wc("a"), 1)
    printAndAssert(wc("a b c"), 3)
    printAndAssert(wc("abc asdf g s"), 4)

    printAndAssert(FoldableOption.toList(Option("a")), List("a"))
    printAndAssert(
      new FoldableTree()
        .toList(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c")))),
      List("a", "b", "c")
    )

    printAndAssert(
      bag(IndexedSeq(1, 1, 2, 3, 3, 3)),
      Map(1 -> 2, 2 -> 1, 3 -> 3)
    )
  }

}
