package com.redbook

import com.redbook.Chapter11.Monad

object Chapter15 {
  sealed trait Process[I, O] {
    import Process._

    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) =>
        s match {
          case h #:: t => recv(Some(h))(t)
          case xs      => recv(None)(xs)
        }
      case Emit(h, t) => h #:: t(s)
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this)
        case Await(recv) =>
          Await {
            case None => recv(None)
            case i    => go(recv(i))
          }
        case Emit(h, t) => Emit(h, go(t))
      }
      go(this)
    }

    // Ex5
    def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
      case Halt()       => Halt()
      case Emit(h2, t2) => Emit(h2, this |> t2)
      case Await(recv2) =>
        this match {
          case Emit(h1, t1) => t1 |> recv2(Some(h1))
          case Await(recv1) => Await(i => recv1(i) |> p2)
          case Halt()       => Halt[I, O]() |> recv2(None)
        }
    }

    def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

    def ++(p: => Process[I, O]): Process[I, O] = this match {
      case Halt()      => p
      case Emit(h, t)  => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
    }

    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
      case Halt()      => Halt()
      case Emit(h, t)  => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))
    }

    // Ex6
    def zipWithIndex: Process[I, (I, Int)] =
      loop(0)((i, s) => ((i, s + 1), s + 1))

    // Ex7
    def zip[O2](p: Process[I, O2]): Process[I, (O, O2)] = this match {
      case Halt()      => Halt()
      case Emit(h, t)  => ???
      case Await(recv) => ???
    }
  }

  case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]())
      extends Process[I, O]
  case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
  case class Halt[I, O]() extends Process[I, O]

  object Process {
    def liftOne[I, O](f: I => O): Process[I, O] = Await {
      case Some(i) => Emit(f(i))
      case None    => Halt()
    }

    def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

    def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _               => Halt()
    }.repeat

    def sum: Process[Double, Double] = {
      def go(acc: Double): Process[Double, Double] = Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None    => Halt()
      }
      go(0.0)
    }

    def monad[I]: Monad[({ type f[x] = Process[I, x] })#f] =
      new Monad[({ type f[x] = Process[I, x] })#f] {
        def unit[O](o: => O): Process[I, O] = Emit(o)

        def flatMap[O, O2](p: Process[I, O])(
            f: O => Process[I, O2]
        ): Process[I, O2] =
          p flatMap f
      }

    // Ex1
    def take[I](n: Int): Process[I, I] = Await[I, I] {
      case None        => Halt()
      case _ if n == 0 => Halt()
      case Some(d)     => Emit(d, take(n - 1))
    }

    def drop[I](n: Int): Process[I, I] = Await[I, I] {
      case None              => Halt()
      case Some(d) if n == 0 => Emit(d, lift(identity))
      case Some(_)           => drop(n - 1)
    }

    def takeWhile[I](f: I => Boolean): Process[I, I] = Await[I, I] {
      case None             => Halt()
      case Some(d) if !f(d) => Halt()
      case Some(d)          => Emit(d, takeWhile(f))
    }

    def dropWhile[I](f: I => Boolean): Process[I, I] = Await[I, I] {
      case None             => Halt()
      case Some(d) if !f(d) => Emit(d, lift(identity))
      case Some(_)          => dropWhile(f)
    }

    // Ex2
    def count[I]: Process[I, Int] = {
      def go(acc: Int): Process[I, Int] = Await {
        case Some(_) => Emit(acc + 1, go(1 + acc))
        case None    => Halt()
      }

      go(0)
    }

    // Ex3
    def mean: Process[Double, Double] = {
      def go(s: Double, c: Int): Process[Double, Double] = {
        Await {
          case Some(d) => Emit((s + d) / c, go(s + d, c))
          case None    => Halt()
        }
      }

      go(0.0, 0)
    }

    def await[I, O](f: I => Process[I, O]): Process[I, O] = Await {
      case None    => Halt()
      case Some(i) => f(i)
    }

    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
      await((i: I) =>
        f(i, z) match {
          case (o, s2) => Emit(o, loop(s2)(f))
        }
      )

    // Ex4
    def sumLoop: Process[Double, Double] =
      loop(0.0)((i, s) => (s + i, s + i))

    def countLoop[I]: Process[I, Int] =
      loop(0)((_, s) => (s + 1, s + 1))

    // Ex7
    def meanZip: Process[Double, Double] =
      sum.zip(count).map { case (s, c) => s / c }

    // Ex8
    def exists[I](f: I => Boolean): Process[I, Boolean] = await { (i: I) =>
      if (f(i)) Emit(true) else exists(f)
    }
  }

  def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

  // Ex9
  def fToC: Process[String, String] =
    Process
      .filter[String](l => l.nonEmpty && !l.startsWith("#"))
      .map(l => toCelsius(l.toDouble).toString)

  sealed trait Process1[F[_], O] {
    import Process1._

    def onHalt(f: Throwable => Process1[F, O]): Process1[F, O] = this match {
      case Process1.Await(req, recv) =>
        Process1.Await(req, recv andThen (_.onHalt(f)))
      case Process1.Emit(h, t) => Emit(h, t.onHalt(f))
      case Process1.Halt(err)  => Try(f(err))
    }

    def ++(p: => Process1[F, O]): Process1[F, O] =
      this.onHalt {
        case End => p
        case err => Halt(err)
      }

    def flatMap[O2](f: O => Process1[F, O2]): Process1[F, O2] =
      this match {
        case Process1.Await(req, recv) =>
          Process1.Await(req, recv andThen (_ flatMap f))
        case Process1.Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
        case Process1.Halt(err)  => Process1.Halt(err)
      }

    // Ex10
    def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
      def go(cur: Process1[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = {
        cur match {
          case Process1.Await(req, recv) =>
            val next =
              try recv(Right(F.attempt(req)))
              catch { case err: Throwable => recv(Left(err)) }
            go(next, acc)
          case Process1.Emit(h, t) => go(t, acc :+ h)
          case Process1.Halt(End)  => F.unit(acc)
          case Process1.Halt(err)  => F.fail(err)
        }
      }
      go(this, IndexedSeq())
    }

    // Ex12
    def join(p: Process1[F, Process1[F, O]]): Process1[F, O] =
      p.flatMap(identity)
  }
  object Process1 {
    case class Await[F[_], A, O](
        req: F[A],
        recv: Either[Throwable, A] => Process1[F, O]
    ) extends Process1[F, O]
    case class Emit[F[_], O](head: O, tail: Process1[F, O])
        extends Process1[F, O]
    case class Halt[F[_], O](err: Throwable) extends Process1[F, O]

    case object End extends Exception

    case object Kill extends Exception

    trait MonadCatch[F[_]] extends Monad[F] {
      def attempt[A](a: F[A]): F[Either[Throwable, A]]

      def fail[A](t: Throwable): F[A]
    }

    def Try[F[_], O](p: => Process1[F, O]): Process1[F, O] =
      try p
      catch {
        case e: Throwable => Halt(e)
      }

    def await[F[_], A, O](req: F[A])(
        recv: Either[Throwable, A] => Process1[F, O]
    ): Process1[F, O] =
      Await(req, recv)

    // Ex11
    def eval[F[_], A](a: F[A]): Process1[F, A] = {
      Await(
        a,
        {
          case Left(err) => Halt(err)
          case Right(v)  => Emit(v, Halt(End))
        }
      )
    }

    def eval_[F[_], A, B](a: F[A]): Process1[F, B] = eval(a) // ???
  }
}
