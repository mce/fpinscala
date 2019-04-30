package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0X5DEECE66DL + 0XBL) & 0XFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, r) => (Int.MaxValue, r)
    case (n, r)            => (Math.abs(n), r)
  }

  def double(rng: RNG): (Double, RNG) =
    map(rng => nonNegativeInt(rng))(a => {
      a / (Int.MaxValue.toDouble + 1)
    })(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = intDouble(rng) match {
    case ((i, d), r) => ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A]))((e, acc) => map2(e, acc)(_ :: _))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    flatMap(nonNegativeInt)(i => {
      rng2 =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) (mod, rng2)
        else nonNegativeLessThan(n)(rng)
    })(rng)
  }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(i => unit(f(i)))
  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(i => {
      State[S, B](s => (f(i), s))
    })
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => {
      sb.map(b => f(a, b))
    })
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B](s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State[Machine, (Int, Int)](initial => {
      val m = inputs.foldLeft(initial)((s, input) =>
        input match {
          case Coin if s.locked && s.candies > 0 => s.copy(locked = false, coins = s.coins + 1)
          case Turn if !s.locked => s.copy(locked = true, candies = s.candies - 1)
          case t => s
        }
      )

      ((m.coins, m.candies), m)
    })
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}
