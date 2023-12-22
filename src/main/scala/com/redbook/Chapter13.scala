package com.redbook

import Chapter11._
import com.redbook.Par.Par

import scala.io.StdIn

object Chapter13 {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F[_], B]): Free[F[_], B] =
      FlatMap(this, f)

    def map[B](f: A => B): Free[F[_], B] =
      flatMap(f andThen (Return[F[_], B](_)))
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B])
      extends Free[F, B]

  // Ex1
  def freeMonad[F[_]] = new Monad[({ type f[a] = Free[F, a] })#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](ma: Free[F, A])(
        f: A => Free[F, B]
    ): Free[F, B] =
      FlatMap(ma.flatMap[B](b => FlatMap(f(b), Return(_))), Return(_))
  }

  type Function0[A] = () => A

  // Ex2
  @annotation.tailrec
  def runTrampoline[A](fa: Free[Function0, A]): A = fa match {
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(x, f: (A => Free[Function0, A])) =>
      x match {
        case Return(a: A)             => runTrampoline(f(a))
        case Suspend(r: Function0[A]) => runTrampoline(f(r()))
        case FlatMap(y, g: (A => Free[Function0, A])) =>
          runTrampoline(y flatMap ((z: A) => (g(z) flatMap f)))
      }
  }

  // Ex3
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(
          FlatMap(x: Free[F, A], f: (A => Free[F, A])),
          g: (A => Free[F, A])
        ) =>
      step((x: Free[F, A]) flatMap ((a: A) => f(a) flatMap g))
    case FlatMap(Return(x: A), f: (A => Free[F, A])) => step(f(x))
    case _                                           => a
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a)  => F.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f: (A => Free[F, A])) =>
      x match {
        case Suspend(r: F[A]) => F.flatMap(r: F[A])((a: A) => run(f(a)))
        case _                => sys.error("Impossible")
      }
  }

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: Function0[A]
  }

  case object ReadLine extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)
    override def toThunk: Function0[Option[String]] = () => run
    def run: Option[String] =
      try Some(StdIn.readLine())
      catch { case e: Exception => None }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit] = Par.lazyUnit(println(line))
    override def toThunk: Function0[Unit] = () => println(line)
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]
    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
  }

  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
  type ~>[F[_], G[_]] = Translate[F, G]

  def consoleToFunction0 = new (Console ~> Function0) {
    override def apply[A](a: Console[A]): Function0[A] = a.toThunk
  }

  def consoleToPar = new (Console ~> Par) {
    override def apply[A](a: Console[A]): Par[A] = a.toPar
  }

  def runFree[F[_], G[_], A](
      free: Free[F, A]
  )(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a)              => G.unit(a)
      case Suspend(r)             => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _                      => sys.error("Impossible")
    }

  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A): Function0[A] = a
    override def flatMap[A, B](ma: Function0[A])(
        f: A => Function0[B]
    ): Function0[B] = () => f(ma())()
  }

  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = Par.fork {
      Par.flatMap(a)(f)
    }
  }

  // Ex4
  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G)(implicit
      G: Monad[G]
  ): Free[G, A] =
    Return(runFree(f)(fg))

  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline(translate(a)(consoleToFunction0))

}
