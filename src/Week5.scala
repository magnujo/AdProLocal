import RNG.SimpleRNG

import scala.::
// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: _____
//
// AUTHOR1: __________
// TIME1: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: __________
// TIME2: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// This file is compiled with 'sbt compile' and tested with 'sbt test'.


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG extends App {

  case class SimpleRNG (seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {

      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

      // The next state, which is an `RNG` instance created from the new seed.
      val nextRNG = SimpleRNG (newSeed)

      // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt

      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG)

    }

  }

  // Exercise 1 (CB 6.1)
  val rng = SimpleRNG(100)

  def nonNegativeInt (rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i<0) -(i + 1) else i, r)
  }
  //print(nonNegativeInt(SimpleRNG(42)))
  // Exercise 2 (CB 6.2)

  def double (rng: RNG): (Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    (i/Int.MaxValue.toDouble, r)
  }

  // Exercise 3 (CB 6.3)

  def intDouble (rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val (i1, r1) = double(r)
    ((i, i1), r1)
  }

  //(intDouble(rng))

  def doubleInt (rng: RNG): ((Double, Int), RNG) =
  {
    val (i, r) = nonNegativeInt(rng)
    val (i1, r1) = double(r)
    ((i1, i), r1)
  }

  def boolean (rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i % 2 == 0, rng2) }

  // Exercise 4 (CB 6.4)

  def ints (count: Int) (rng: RNG) : (List[Int], RNG) = {

    def go(count: Int, z: List[Int]) (rng: RNG): (List[Int], RNG) = {
      val (i1, rng2) = rng.nextInt
      if (count == 0) (z, rng2) else go(count - 1, i1 :: z) (rng2)
    }
    (go(count, List[Int]()) (rng))
  }


  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A] (a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B] (s: Rand[A]) (f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s (rng)
      (f (a), rng2)
    }
  def nonNegativeEven: Rand[Int] = map (nonNegativeInt) (i => i - i % 2)

  // Exercise 5 (CB 6.5) (Lazy is added so that the class does not fail
  // at load-time without your implementation).

  lazy val _double: Rand[Double] =
    map (nonNegativeInt) (x => x / ( Int.MaxValue.toDouble + 1))

 // print(unit(42)(SimpleRNG(42)))
  // Exercise 6 (CB 6.6)

  def map2[A,B,C] (ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra (rng)
      val (b, rng3) = rb (rng2)
      (f(a,b),rng3)
    }

  // this is given in the book

  def both[A,B] (ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2 (ra, rb) ((_, _))

  lazy val randIntDouble: Rand[(Int, Double)] = both (int, double)

  lazy val randDoubleInt: Rand[(Double, Int)] = both (double, int)

  val test = SimpleRNG(42)

  def random_list[A] (n: Int) (ra: Rand[A]): Rand[List[A]] = {
    val l = List.fill(n)(ra)
    rng => (l, rng)
  }

  // Exercise 7 (6.7)

  def sequence[A] (fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())) ((a, z) => map2 (a,z) ((a,z) => a :: z))

  def _ints (count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  print(_double(test))

  // Exercise 8 (6.8)

  def flatMap[A,B] (f: Rand[A]) (g: A => Rand[B]): Rand[B] = ???


  def nonNegativeLessThan (n: Int): Rand[Int] = ???

  //print(Int.MaxValue)

  // (if (i <0) -(i +1) else i, r)

  // -(-2147483648 + 1) = 2147483648 - 1 = int Max
  //int Min = -2147483648
  //int Max = 2147483647

   // val (n1, rng2) = rng.nextInt
    //print(rng.nextInt)
   // print(rng.nextInt)
    //print(SimpleRNG(1059025964525).nextInt)



}

//mport State._
//import adpro.RNG.SimpleRNG

case class State[S, +A](run: S => (A, S)) {

  // Exercise 9 (6.10)

  def map[B] (f: A => B): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

  def map2[B,C] (sb: State[S, B]) (f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B] (f: A => State[S, B]): State[S, B] = ???

}

object State extends App {

  //import adpro.Stream

  type Rand[A] = State[RNG, A]

  def unit[S, A] (a: A): State[S, A] =
    State (s => (a, s))

  // Exercise 9 (6.10) continued

  def sequence[S,A] (sas: List[State[S, A]]): State[S, List[A]] = ???

  // This is given in the book:

  def modify[S] (f: S => S): State[S, Unit] = for {
    // Get the current state and assigns it to `s`.
    s <- get
    // Set the new state to `f` applied to `s`.
    _ <- set (f (s))
  } yield ()

  def get[S]: State[S, S] = State (s => (s, s))

  def set[S] (s: S): State[S, Unit] = State (_ => ((), s))

  def random_int: Rand[Int] =  State (_.nextInt)

  // Exercise 10

  def state2stream[S,A] (s: State[S,A]) (seed: S): Stream[A] = ???

  // Exercise 11 (lazy is added so that the class does not crash at load time
  // before you provide an implementation).


  lazy val random_integers = ???

}
