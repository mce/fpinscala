package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(head, t) => Cons(h, t)
    case Nil           => Cons(h, Nil)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ if n == 0      => l
    case Cons(head, tail) => drop(tail, n - 1)
    case Nil              => Nil
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if f(head) => dropWhile(tail)(f)
    case _                           => l
  }

  def init[A](l: List[A]): List[A] = {
    def takeUntilLast(list: List[A], acc: List[A]): List[A] = list match {
      case Cons(head, Nil)  => acc
      case Nil              => acc
      case Cons(head, tail) => takeUntilLast(tail, append(acc, Cons(head, Nil)))
    }

    takeUntilLast(l, Nil)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((elem, size) => size + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(list: List[A], acc: B): B = list match {
      case Nil              => acc
      case Cons(head, tail) => go(tail, f(acc, head))
    }

    go(l, z)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def length3[A](l: List[A]): Int = foldLeft(l, 0)((size, elem) => size + 1)

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, elem) => append(Cons(elem, Nil), acc))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    reverse(foldLeft(a2, reverse(a1))((acc, elem) => Cons(elem, acc)))

  def concatenate[A](as: List[List[A]]): List[A] = {
    reverse(
      foldLeft(as, List[A]())((acc, elem) => {
        foldLeft(elem, acc)((acc2, e) => {
          Cons(e, acc2)
        })
      })
    )
  }

  def plusOne(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((elem, acc) => Cons(elem + 1, acc))
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((elem, acc) => Cons(elem.toString, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((elem, acc) => Cons(f(elem), acc))
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, List[A]())(
      (elem, acc) => if (f(elem)) Cons(elem, acc) else acc
    )
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concatenate(map(as)(f))
  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(e => if (f(e)) List(e) else Nil)
  def listSum(a: List[Int], b: List[Int]): List[Int] = zipWith(a, b)(_ + _)

  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] =
    (a, b) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case (Nil, l)                     => l
      case (l, Nil)                     => l
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case _ if sup == sub => true
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 == h2 && t2 == Nil) true 
        else if (h1 == h2) hasSubsequence(t1, t2) 
        else hasSubsequence(t1, sub)
      case _ => false
    }
  }
}
