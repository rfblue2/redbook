package com.redbook

import Chapter11._

object Chapter12 {
  trait Applicative[F[_]] extends Functor[F] { self =>
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def unit[A](a: => A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

    // Ex1
    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(identity)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(fa))

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      map2(fa, fb)((_, _))

    // Ex4
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fab, fa)((ab, a) => ab(a))

    def mapByApply[A, B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)

    def map2ByApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    // Ex3
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(
        f: (A, B, C) => D
    ): F[D] =
      apply(apply(map(fa)(f.curried))(fb))(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
        f: (A, B, C, D) => E
    ): F[E] =
      apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

    // Ex8
    def product[G[_]](G: Applicative[G]) =
      new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
        override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(
            f: (A, B) => C
        ): (F[C], G[C]) =
          (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))

        override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      }

    // Ex9
    def compose[G[_]](G: Applicative[G]) =
      new Applicative[({ type f[x] = F[G[x]] })#f] {
        override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(
            f: (A, B) => C
        ): F[G[C]] =
          self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))

        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      }

    // Ex10
    // too challenging :(

    // Ex11
    // impossible lol

    // Ex12
    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
      ofa.foldRight(unit(Map.empty[K, V])) { case ((k, fa), fbs) =>
        map2(fa, fbs)((a, b) => b + (k -> a))
      }

  }

  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)
    def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  /* Ex4
   Converts a list of streams into a single stream that returns a list of the next values of each stream on each evaluation
   */

  // Ex5
  def eitherMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](
        ma: Either[E, A]
    )(f: A => Either[E, B]): Either[E, B] =
      ma match {
        case Left(e)  => Left(e)
        case Right(a) => f(a)
      }
  }

  // Ex6
  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector())
      extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
          f: (A, B) => C
      ): Validation[E, C] =
        (fa, fb) match {
          case (Failure(h1, t1), Failure(h2, t2)) =>
            Failure(h1, h2 +: (t1 ++ t2))
          case (e @ Failure(_, _), Success(_)) => e
          case (Success(_), e @ Failure(_, _)) => e
          case (Success(a), Success(b))        => Success(f(a, b))
        }
    }

  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
    p match { case (a, (b, c)) => ((a, b), c) }

  /* Ex7
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def map2ByFlatMap[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => flatMap(mb)(b => f(a, b)))

  Associativity
  compose(compose(f, g), h) == compose(f, compose(g, h))
  <=>
  product(product(fa,fb),fc) == map(product(fa, product(fb, fc)))(assoc)
   */

  type Const[M, B] = M

  trait Traverse[F[_]] extends Functor[F] { self =>
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B])(implicit
        G: Applicative[G]
    ): G[F[B]] =
      sequence(map(fa)(f))

    def sequence[G[_]: Applicative, A](fga: F[G[A]])(implicit
        G: Applicative[G]
    ): G[F[A]] =
      traverse(fga)(ga => ga)

    // Ex14
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      traverse(fa)(f)(Monad.identityMonad)

    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)((a: A) =>
        (for {
          s1 <- State.get[S]
          (b, s2) = f(a, s1)
          _ <- State.set(s2)
        } yield b)
      ).run(s)

    def toList[A](fa: F[A]): List[A] =
      mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

    // Ex16
    def reverse[A](fa: F[A]): F[A] =
      mapAccum(fa, toList(fa).reverse)((a, s) => (s.head, s.tail))._1

    // Ex17
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      mapAccum(as, z)((a, b) => ((), f(b, a)))._2

    // Ex18
    def fuse[G[_], H[_], A, B](fa: F[A])(
        f: A => G[B],
        g: A => H[B]
    )(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
      (
        traverse(fa)(a => G.map(G.unit(a))(f)),
        traverse(fa)(a => H.map(H.unit(a))(g))
      )
    }

    // Ex19
    def compose[G[_]](implicit G: Traverse[G]) =
      new Traverse[({ type f[x] = F[G[x]] })#f] {
        override def traverse[G[_]: Applicative, A, B](
            fa: F[G[A]]
        )(f: A => G[B])(implicit G: Applicative[G]): G[F[G[B]]] = {
          self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
        }
      }

    // Ex20
    // hell no
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

//  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
//    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // Ex13
  val listTraverse = new Traverse[List] {
    override def traverse[G[_]: Applicative, A, B](
        fa: List[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldLeft(G.unit(List.empty[B]))((fbs, a) => G.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B])(
        implicit G: Applicative[G]
    ): G[Option[B]] = {
      fa match {
        case Some(value) => G.map(f(value))(Some.apply)
        case None        => G.unit(None)
      }
    }
  }

  val treeTraverse = new Traverse[Tree] {
    def traverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[B])(implicit
        G: Applicative[G]
    ): G[Tree[B]] = {
      G.map2(f(fa.head), listTraverse.traverse(fa.tail)(a => traverse(a)(f)))(
        Tree(_, _)
      )
    }
  }
}
