package fpinscala.errorhandling

import scala.{
  Option => _,
  Some => _,
  Either => _,
  _
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None        => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None        => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None    => ob
    case Some(v) => Some(v)
  }

  def filter(f: A => Boolean): Option[A] =
    flatMap(value => if (f(value)) Some(value) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] =
    for {
      m <- mean(xs)
      elems = xs.map(d => Math.pow(d - m, 2))
      result <- mean(elems)
    } yield result

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      av <- a
      bv <- b
    } yield f(av, bv)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @annotation.tailrec
    def go(list: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = {
      list match {
        case Nil             => acc
        case None :: tail    => None
        case Some(v) :: tail => go(tail, Some(acc.getOrElse(List[A]()) :+ v))
      }
    }
    go(a, None)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def go(list: List[A], acc: Option[List[B]]): Option[List[B]] = {
      list match {
        case Nil => acc
        case value :: tail =>
          f(value).flatMap(bv => go(tail, Some(acc.getOrElse(List[B]()) :+ bv)))
      }
    }
    go(a, None)
  }
}
