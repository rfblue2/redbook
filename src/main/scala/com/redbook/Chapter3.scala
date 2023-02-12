package com.redbook

object Chapter3 {
  // List
  sealed trait List[+A]
  case object Nil extends List[Nothing] {
    override def toString: String = ""
  }
  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    override def toString: String = {
      s"$head,${tail.toString}"
    }
  }

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Tree
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
}

object Chapter3Ex1 {

  import Chapter3._
  // eval to 3
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(4, Cons(2, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + List.sum(t)
    case _                                     => 101
  }

  def main(args: Array[String]): Unit = {
    println(x)
  }
}

object Chapter3Ex2 {
  import Chapter3._

  implicit class ListTail[A](list: List[A]) {
    def tail: List[A] = list match {
      case Nil         => Nil
      case Cons(x, xs) => xs
    }
  }

  def main(args: Array[String]): Unit = {
    val l1 = List()
    val l2 = List(1, 2, 3)

    println(l1.tail)
    println(l2.tail)
  }

}

object Chapter3Ex3 {
  import Chapter3._

  implicit class ListSetHead[A](list: List[A]) {
    def setHead(v: A): List[A] = list match {
      case Nil         => Nil
      case Cons(x, xs) => Cons(v, xs)
    }
  }

  def main(args: Array[String]): Unit = {
    val l1 = List()
    val l2 = List(1, 2, 3)

    println(l1.setHead(3))
    println(l2.setHead(3))
  }
}

object Chapter3Ex4 {
  import Chapter3._

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _)         => Nil
    case (_, 0)           => l
    case (Cons(x, xs), n) => drop(xs, n - 1)
  }

  def main(args: Array[String]): Unit = {
    val l1 = List()
    val l2 = List(1, 2, 3)

    println(drop(l1, 2))
    println(drop(l2, 2))
  }
}

object Chapter3Ex5 {
  import Chapter3._

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil                 => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _                   => l
  }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil                 => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _                   => l
  }

  def main(args: Array[String]): Unit = {
    val l1 = List()
    val l2 = List(1, 2, 3)

    println(dropWhile(l1, (x: Int) => x <= 2))
    println(dropWhile(l2, (x: Int) => x <= 2))
    println(dropWhile2(l2)(_ <= 2))
  }
}

object Chapter3Ex6 {
  import Chapter3._

  def init[A](l: List[A]): List[A] = l match {
    case Nil | Cons(_, Nil) => Nil
    case Cons(x, xs)        => Cons(x, init(xs))
  }

  def main(args: Array[String]): Unit = {
    val l1 = List()
    val l2 = List(1, 2, 3)

    println(init(l1))
    println(init(l2))
  }
}

object Chapter3Ex7 {
  import Chapter3._

  def main(args: Array[String]): Unit = {
    // no way to short circuit b/c you cannot replace the list argument of foldRight after
    // the first outer invocation
    // probably way to short circuit is to include a callback as part of (z) and continue logic
    // as part of that callback
    def fastProduct(ns: List[Double]) = {
      foldRight(ns, 1.0)(_ * _)
    }
  }
}

object Chapter3Ex8 {
  import Chapter3._

  def main(args: Array[String]): Unit = {
    // just makes a list
    println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
  }
}

object Chapter3Ex9 {
  import Chapter3._

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, l) => l + 1)

  def main(args: Array[String]): Unit = {
    println(length(List()))
    println(length(List(1, 2, 3)))
  }
}

object Chapter3Ex10 {
  import Chapter3._

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
}

object Chapter3Ex11 {
  import Chapter3._
  import Chapter3Ex10._

  def product(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def sum(ns: List[Double]) =
    foldLeft(ns, 0.0)(_ + _)

  def length[A](ns: List[A]) =
    foldLeft(ns, 0.0)((x, _) => x + 1)

  def main(args: Array[String]): Unit = {
    println(product(List()))
    println(product(List(1, 2, 3)))
    println(product(List(1, 0, 3)))

    println(sum(List()))
    println(sum(List(1, 2, 3)))
    println(sum(List(1, 0, 3)))

    println(length(List()))
    println(length(List(1, 2, 3)))
    println(length(List(1, 0, 3)))
  }
}

object Chapter3Ex12 {
  import Chapter3._
  import Chapter3Ex10._

  def reverse[A](ns: List[A]): List[A] =
    foldLeft(ns, Nil: List[A])((a, b) => Cons(b, a))

  def main(args: Array[String]): Unit = {
    println(reverse(List(1, 2, 3)))
  }
}

object Chapter3Ex13 {
  import Chapter3._
  import Chapter3Ex10._
  import Chapter3Ex12._

  def newFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def newFoldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def main(args: Array[String]): Unit = {
    println(foldRight(List(10, 5, 2), 0)(_ - _))
    println(newFoldRight(List(10, 5, 2), 0)(_ - _))
    println(foldLeft(List(10, 5, 2), 0)(_ - _))
    println(newFoldLeft(List(10, 5, 2), 0)(_ - _))
  }
}

object Chapter3Ex14 {
  import Chapter3._

  def append[A](as: List[A], x: A): List[A] =
    foldRight(as, List(x))(Cons(_, _))

  def main(args: Array[String]): Unit = {
    println(append(Nil, 1))
    println(append(List(1, 2, 3), 4))
  }
}

object Chapter3Ex15 {
  import Chapter3._

  def concat[A](lls: List[List[A]]): List[A] =
    foldRight(lls, Nil: List[A])((ls, acc) => foldRight(ls, acc)(Cons(_, _)))

  def main(args: Array[String]): Unit = {
    println(concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8))))
  }
}

object Chapter3Ex16 {
  import Chapter3._

  def inc(l: List[Int]): List[Int] =
    l match {
      case Nil              => Nil
      case Cons(head, tail) => Cons(head + 1, inc(tail))
    }

  def main(args: Array[String]): Unit = {
    println(inc(List(1, 2, 3)))
  }
}

object Chapter3Ex17 {
  import Chapter3._

  def stringify(l: List[Double]): List[String] =
    l match {
      case Nil              => Nil
      case Cons(head, tail) => Cons(head.toString, stringify(tail))
    }

  def main(args: Array[String]): Unit = {
    println(stringify(List(1, 2, 3)))
  }
}

object Chapter3Ex18 {
  import Chapter3._

  def map[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil              => Nil
      case Cons(head, tail) => Cons(f(head), map(tail)(f))
    }

  def inc(as: List[Int]): List[Int] = map(as)(_ + 1)

  def main(args: Array[String]): Unit = {
    println(inc(List(1, 2, 3)))
  }
}

object Chapter3Ex19 {
  import Chapter3._

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (f(head)) Cons(head, filter(tail)(f))
        else filter(tail)(f)
    }

  def main(args: Array[String]): Unit = {
    println(filter(List(1, 2, 3, 4))(_ % 2 == 0))
  }
}

object Chapter3Ex20 {
  import Chapter3._
  import Chapter3Ex12._
  import Chapter3Ex15._

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case Nil => Nil
      case Cons(head, tail) =>
        concat(List(f(head), flatMap(tail)(f)))
    }

  def fill(n: Int): List[Int] = {
    def aux(i: Int): List[Int] =
      i match {
        case 0 => Nil
        case n => Cons(i, aux(n - 1))
      }
    reverse(aux(n))
  }

  def main(args: Array[String]): Unit = {
    println(fill(3))
    println(flatMap(List(1, 2, 3))(fill))
  }
}

object Chapter3Ex21 {
  import Chapter3._
  import Chapter3Ex20._

  def newFilter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else List())

  def main(args: Array[String]): Unit = {
    println(newFilter(List(1, 2, 3, 4))(_ % 2 == 0))
  }
}

object Chapter3Ex22 {
  import Chapter3._

  def add(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, Nil)                     => Nil
      case (Cons(x1, xs1), Nil)           => Cons(x1, add(xs1, l2))
      case (Nil, Cons(x2, xs2))           => Cons(x2, add(l1, xs2))
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, add(xs1, xs2))
    }
  }

  def main(args: Array[String]): Unit = {
    println(add(List(1, 2, 3), List(4, 5, 6)))
  }
}

object Chapter3Ex23 {
  import Chapter3._

  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = {
    (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) =>
        Cons(f(x1, x2), zipWith(xs1, xs2)(f))
    }
  }

  def add(l1: List[Int], l2: List[Int]): List[Int] = zipWith(l1, l2)(_ + _)

  def main(args: Array[String]): Unit = {
    println(add(List(1, 2, 3), List(4, 5, 6)))
  }
}

object Chapter3Ex24 {
  import Chapter3._

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys)) =>
        x == y && hasSubsequence(xs, ys) || hasSubsequence(xs, sub)
    }
  }

  def main(args: Array[String]): Unit = {
    // true
    println(hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2, 3)))
    println(hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4)))
    println(hasSubsequence(List(1, 2, 3, 4, 5), List(3, 4)))
    println(hasSubsequence(List(1, 2, 3, 4, 5), Nil))

    // false
    println(hasSubsequence(Nil, List(1)))
    println(hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 1)))
  }
}

object Chapter3Ex25 {
  import Chapter3._

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(value)         => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def main(args: Array[String]): Unit = {
    println(size(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(2))), Leaf(1))))
  }
}

object Chapter3Ex26 {
  import Chapter3._

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value)         => value
    case Branch(left, right) => max(left).max(max(right))
  }

  def main(args: Array[String]): Unit = {
    println(max(Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(2))), Leaf(1))))
  }
}

object Chapter3Ex27 {
  import Chapter3._

  def depth(tree: Tree[Int]): Int = tree match {
    case Leaf(value)         => 1
    case Branch(left, right) => depth(right).max(depth(left)) + 1
  }

  def main(args: Array[String]): Unit = {
    println(depth(Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(2))), Leaf(1))))
  }
}

object Chapter3Ex28 {
  import Chapter3._

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def main(args: Array[String]): Unit = {
    println(
      map(Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(2))), Leaf(1)))(_ + 1)
    )
  }
}

object Chapter3Ex29 {
  import Chapter3._
  import Chapter3Ex25._
  import Chapter3Ex26._
  import Chapter3Ex27._
  import Chapter3Ex28._

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (Tree[A], Tree[A]) => B): B =
    tree match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => g(left, right)
    }

  def max2(tree: Tree[Int]): Int =
    fold(tree)(identity)((l, r) => max2(l).max(max2(r)))

  def size2[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((l, r) => size2(l) + size2(r))

  def depth2[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((l, r) => depth2(l).max(depth2(r)) + 1)

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B]) { (l, r) =>
      Branch(map2(l)(f), map2(r)(f))
    }

  def main(args: Array[String]): Unit = {
    val tree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(2))), Leaf(1))
    println(size(tree))
    println(size2(tree))

    println(depth(tree))
    println(depth2(tree))

    println(max(tree))
    println(max2(tree))

    println(map(tree)(_ + 1))
    println(map2(tree)(_ + 1))
  }
}
