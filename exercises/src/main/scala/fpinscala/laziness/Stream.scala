package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty            => Nil
    case Cons(head, tail) => head() :: tail().toList
  }

  def take(n: Int): Stream[A] = n match {
    case 0 => Empty
    case _ =>
      this match {
        case Empty            => Empty
        case Cons(head, tail) => Cons(head, () => tail().take(n - 1))
      }
  }

  def drop(n: Int): Stream[A] = n match {
    case 0 => this
    case _ =>
      this match {
        case Empty            => Empty
        case Cons(head, tail) => tail().drop(n - 1)
      }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) =>
      if (p(head())) Cons(head, () => tail().takeWhile(p)) else Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty            => true
    case Cons(head, tail) => p(head()) && tail().forAll(p)
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    this.foldRight(empty[A])((elem, acc) => {
      if (p(elem)) Cons(() => elem, () => acc) else empty
    })
  }

  def headOption: Option[A] =
    this.foldRight[Option[A]](None)((elem, acc) => Some(elem))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((e, acc) => Cons(() => f(e), () => acc))
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((e, acc) => if (f(e)) Cons(() => e, () => acc) else acc)
  def append[B >: A](elem: => Stream[B]): Stream[B] =
    foldRight(elem)((e, acc) => cons(e, acc))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this)(
      s =>
        s match {
          case Cons(h, t) => Some(f(h()), t())
          case Empty      => None
        }
    )

  def takeUnfold(n: Int): Stream[A] =
    unfold(this)(
      s =>
        s match {
          case Cons(h, t) if (n > 0) => Some(h(), t().takeUnfold(n - 1))
          case _                     => None
        }
    )

  def takeWhileUnfold(p: A => Boolean): Stream[A] = {
    unfold(this)(
      s =>
        s match {
          case Cons(h, t) => if (p(h())) Some(h(), t().takeWhileUnfold(p)) else None
          case _ => None
        }
    )
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold( (this, s2)) {
    case (Cons(h, t), Cons(h2, t2)) => Some((f(h(), h2()), (t(), t2())))
    case _ => None
  }


  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold( (this, s2)) {
    case (Cons(h, t), Cons(h2, t2)) => Some((Some(h()), Some(h2())), (t(), t2()))
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty[B]))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty[A], t2()))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = zipWith(s)( (a,b) => (a,b)).forAll(t => t._1 == t._2)
  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some((Cons(h, t), t()))
    case Empty => None
  } append Stream(empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def fibs: Stream[Int] = {
    def go(f1: Int, f2: Int): Stream[Int] = {
      cons(f1, (go(f2, f1 + f2)))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None         => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  def ones2: Stream[Int] = unfold(1)(i => Some(i, i + 1))
  def from2(n: Int): Stream[Int] = unfold(n)(i => Some(i, i + 1))
  def constant2[A](a: A): Stream[A] = unfold(a)(i => Some(i, i))
  def fibs2: Stream[Int] = unfold((0, 1)) { case (f, s) => Some(f, (s, s + f)) }
}
