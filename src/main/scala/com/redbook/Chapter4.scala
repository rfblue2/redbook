package com.redbook

object Chapter4Ex1 {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
  }

  case object None extends Option[Nothing] {
    override def map[B](f: Nothing => B): Option[B] = None

    override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

    override def filter(f: Nothing => Boolean): Option[Nothing] = None

    override def getOrElse[B >: Nothing](default: => B): B = default

    override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  }

  case class Some[+A](v: A) extends Option[A] {
    override def map[B](f: A => B): Option[B] = Some(f(v))

    override def flatMap[B](f: A => Option[B]): Option[B] = f(v)

    override def filter(f: A => Boolean): Option[A] =
      if (f(v)) Some(v) else None

    override def getOrElse[B >: A](default: => B): B = v

    override def orElse[B >: A](ob: => Option[B]): Option[B] = Some(v)
  }

  def main(args: Array[String]): Unit = {
    println((for {
      x <- Some(1)
      y <- Some(2)
    } yield x + y).getOrElse(4)) // 3

    println((for {
      x <- Some(1).filter(_ % 2 == 0)
      y <- Some(2)
    } yield x + y).getOrElse(4)) // 4

    println((for {
      x <- Some(1).filter(_ % 2 == 0)
      y <- Some(2)
    } yield x + y).orElse(Some(5).filter(_ % 2 == 1))) // Some(5)
  }
}

object Chapter4Ex2 {
  import Chapter4Ex1._
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def main(args: Array[String]): Unit = {
    println(variance(Seq()))
    println(variance(Seq(1, 2, 3)))
  }
}

object Chapter4Ex3 {

  import Chapter4Ex1._

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def main(args: Array[String]): Unit = {
    println(map2(Some(3), Some(4))(_ + _))
  }
}

object Chapter4Ex4 {

  import Chapter4Ex1._

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(List[A]()): Option[List[A]]) { (next, acc) =>
      for {
        l <- acc
        n <- next
      } yield n :: l
    }

  def main(args: Array[String]): Unit = {
    println(sequence(List(Some(1), Some(2), Some(3))))
    println(sequence(List(Some(1), None, Some(3))))
  }
}

object Chapter4Ex5 {

  import Chapter4Ex1._

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List[B]()): Option[List[B]]) { (next, acc) =>
      for {
        l <- acc
        n <- f(next)
      } yield n :: l
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

  def main(args: Array[String]): Unit = {
    println(sequence(List(Some(1), Some(2), Some(3))))
    println(sequence(List(Some(1), None, Some(3))))

    println(traverse(List(1, 2, 3)) { x => Some(x + "") })
    println(traverse(List(1, 2, 3)) { x => Some(x).filter(_ % 2 == 1) })
  }
}

object Chapter4Ex6 {

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {
    override def map[B](f: Nothing => B): Either[E, B] = Left(value)

    override def flatMap[EE >: E, B](
        f: Nothing => Either[EE, B]
    ): Either[EE, B] = Left(value)

    override def orElse[EE >: E, B >: Nothing](
        b: => Either[EE, B]
    ): Either[EE, B] = b

    override def map2[EE >: E, B, C](b: Either[EE, B])(
        f: (Nothing, B) => C
    ): Either[EE, C] = Left(value)
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))

    override def flatMap[EE >: Nothing, B](
        f: A => Either[EE, B]
    ): Either[EE, B] = f(value)

    override def orElse[EE >: Nothing, B >: A](
        b: => Either[EE, B]
    ): Either[EE, B] = Right(value)

    override def map2[EE >: Nothing, B, C](b: Either[EE, B])(
        f: (A, B) => C
    ): Either[EE, C] =
      b.map { x => f(value, x) }
  }

  def main(args: Array[String]): Unit = {
    println(for {
      x <- Right(1)
      y <- Right(2)
    } yield x + y)

    println(for {
      x <- Right(1)
      y <- Left("error"): Either[String, Int]
    } yield x + y)
  }
}

object Chapter4Ex7 {

  import Chapter4Ex6._

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(List[B]()): Either[E, List[B]]) { (next, acc) =>
      for {
        l <- acc
        n <- f(next)
      } yield n :: l
    }

  def main(args: Array[String]): Unit = {
    println(sequence(List(Right(1), Right(2), Right(3))))
    println(sequence(List(Right(1), Left("err"), Right(3))))

    println(traverse(List(1, 2, 3)) { x => Right(x + "") })
    println(traverse(List(1, 2, 3)) { x =>
      if (x % 2 == 1) Right(x) else Left("even")
    })
  }
}

object Chapter4Ex8 {

  // You would need a data type that continues to map collecting failures

}
