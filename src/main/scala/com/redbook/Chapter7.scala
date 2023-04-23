package com.redbook

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{
  Callable,
  CountDownLatch,
  ExecutorService,
  Executors,
  Future,
  TimeUnit
}
import scala.collection.mutable.ListBuffer
import scala.util.Try

trait Chapter7Ex1 {
  trait Par[A]
  def unit[A](a: => A): Par[A]
  def get[A](a: Par[A]): A
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

  // typechecks!
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(sum(l), sum(r))(_ + _)
    }
}

object Chapter7Ex2 {
  sealed trait Par[A]
  case class Imm[A](x: A) extends Par[A]
  case class Def[A](x: () => Par[A]) extends Par[A]

  def unit[A](a: A): Par[A] = Imm(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (a, b) match {
      case (Imm(a), Imm(b)) => Imm(f(a, b))
      case (Def(a), Imm(b)) => Def(() => map2(a(), Imm(b))(f))
      case (Imm(a), Def(b)) => Def(() => map2(Imm(a), b())(f))
      case (Def(a), Def(b)) => Def(() => map2(a(), b())(f))
    }

  def fork[A](a: => Par[A]): Par[A] = Def(() => a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = a match {
    case Def(x) => run(x())
    case Imm(x) => x
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    println(ints)
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }
  }

  def sumNoFork(ints: IndexedSeq[Int]): Par[Int] = {
    println(ints)
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(sum(l), sum(r))(_ + _)
    }
  }

  def main(args: Array[String]): Unit = {
    run(sum(IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8)))

    /* Vector(1, 2, 3, 4, 5, 6, 7, 8)
     * Vector(1, 2, 3, 4)
     * Vector(5, 6, 7, 8)
     * Vector(1, 2)
     * Vector(3, 4)
     * Vector(5, 6)
     * Vector(7, 8)
     * Vector(1)
     * Vector(2)
     * Vector(3)
     * Vector(4)
     * Vector(5)
     * Vector(6)
     * Vector(7)
     * Vector(8)
     */
    println()
    run(sumNoFork(IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8)))
  }
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        override def call(): A = a(es).get
      })

  // Ex 4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // Ex 5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case ::(p, next) => map2(fork(p), sequence(next))(_ :: _)
      case Nil         => unit(List())
    }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // Ex 6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as match {
      case ::(a, next) =>
        map2(fork(unit(f(a))), parFilter(next)(f))((x, l) =>
          if (x) a :: l else l
        )
      case Nil => unit(List())
    }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  // Ex 11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices(run(es)(n).get)(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(x => if (x) 0 else 1))(List(t, f))

  // Ex 12
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(run(es)(key).get)(es)

  // Ex 13
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(run(es)(pa).get)(es)

  // Ex 14
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(a).get()(es)

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

  def main(args: Array[String]): Unit = {
    def iVal2[A, B](b: B, a: A): A = {
      println(b)
      Thread.sleep(1000)
      println(b)
      a
    }

    def iVal[A](a: A): A = {
      println(a)
      Thread.sleep(1000)
      println(a)
      a
    }

    val exec = Executors.newFixedThreadPool(11)

    val p = map2(fork(unit(iVal(1))), fork(unit(iVal(2))))((a, b) => a + b)
    val res = p(exec).get()
    println(res)

    println()

    val p2 = parMap(List(1, 2, 3, 4, 5))(x => iVal2(x, x + 1))
    val res2 = p2(exec).get()
    println(res2)

    println()

    val p3 = parFilter(List(1, 2, 3, 4, 5))(x => {
      iVal2(x, x % 2 == 0)
    })
    val res3 = p3(exec).get()
    println(res3)

    println()

    def sum(ints: IndexedSeq[Int]): Par[Int] = {
      if (ints.length <= 1)
        unit(ints.headOption.getOrElse(0))
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        map2(fork(sum(l)), fork(sum(r)))(_ + _)
      }
    }

    println(sum(IndexedSeq(1, 2, 3, 4, 5))(exec).get())

    println()

    val p4 = choice(Par.unit(true))(Par.unit(1), Par.unit(2))
    println(p4(exec).get)

    println()

    val p5 = choiceMap(Par.unit(1))(Map(1 -> Par.unit(1), 2 -> Par.unit(2)))
    println(p5(exec).get)

    exec.shutdown()
  }
}

// TODO
object Chapter7Ex3 {}

// Ex7
/*
 * f compose g == x => f(g(x))
 * map(y)(id) == y
 * id(a) == a
 *
 * map(map(y)(g))(f) == map(y)(f compose g)
 * map(map(y)(g))(f) == map(y)(x => f(g(x)))
 * let f = id
 *
 * map(map(y)(g))(id) == map(y)(x => id(g(x)))
 * map(y)(g) == map(y)(x => g(x))
 * qed?
 */

// Ex8,9
/*
 * Deadlocking on single threadpool.
 * Any size can deadlock b/c you can just open more threads that wait on other threads
 * by wrapping in another fork layer
 */
object Chapter7Ex9 {
  import Par._
  def main(args: Array[String]): Unit = {
    val a = lazyUnit(1)
    val S = Executors.newFixedThreadPool(2)
    println(Par.equal(S)(fork(a), fork(fork(a))))
    S.shutdown()
  }
}

// still hangs :(
object Chapter7Ex10 {
  sealed trait Future[+A] {
    def apply(k: A => Unit)(e: Throwable => Unit): Unit
  }
  type Par[+A] = ExecutorService => Future[A]

  def run[A](
      es: ExecutorService
  )(p: Par[A], e: Throwable => Unit = throw _): A = {
    val ref = new AtomicReference[A]()
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }(ex => {
      println(ex)
      latch.countDown()
      e(ex)
    })
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit)(e: Throwable => Unit): Unit = try {
          cb(a)
        } catch {
          case ex: Throwable =>
            println(ex)
            e(ex)
        }
      }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit)(e: Throwable => Unit): Unit = try {
          eval(es)(a(es)(cb)(e))
        } catch {
          case ex: Throwable => {
            println(ex)
            e(ex)
          }
        }
      }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit]() { def call = r })

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es =>
    new Future[C] {
      def apply(cb: C => Unit)(e: Throwable => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A, B]](es)(
          {
            case Left(a) =>
              br match {
                case None    => ar = Some(a)
                case Some(b) => eval(es)(cb(f(a, b)))
              }
            case Right(b) =>
              ar match {
                case None    => br = Some(b)
                case Some(a) => eval(es)(cb(f(a, b)))
              }
          },
          e
        )
        p(es)(a => combiner ! Left(a))(e)
        p2(es)(b => combiner ! Right(b))(e)
      }
    }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case ::(p, next) => map2(fork(p), sequence(next))(_ :: _)
      case Nil         => unit(List())
    }

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(2)
    val p = parMap(List.range(1, 10))(x => {
      println(x)
      val r = math.sqrt(x)
      if (x == 5)
        throw new RuntimeException("fail!")
      println(x)
      r
    })
    val x = run(es)(p, e => println(e.getMessage))
    println(x.take(10))
    es.shutdown()
  }
}
