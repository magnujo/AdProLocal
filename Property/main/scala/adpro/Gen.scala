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
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.    The tests
// will fail for unfnished parts.  Comment such out.
//
// Before starting to work on the exercises, familiarize yourself with the the
// content already in the file (it has been explained in chapter 8, but it is
// useful to see it all together in one file).

package adpro

import java.security.KeyStore.TrustedCertificateEntry

import fpinscala.laziness.Stream._
import fpinscala.laziness.Stream
import fpinscala.state.{RNG, _}
import fpinscala.state.RNG.{nonNegativeInt, _}

object WarmupExercises extends App{


  // Exercise 1
  lazy val rng1: RNG = Simple(42)

  // Exercise 2
  lazy val x: Int = rng1.nextInt._1
  lazy val s: RNG = rng1.nextInt._2
  lazy val y: Int = s.nextInt._1

  // Exercise 3
  lazy val s_random_int: State[RNG,Int] = State(s => s.nextInt)
  lazy val s_nonNegativeInt: State[RNG,Int] = State(s => RNG.nonNegativeInt(s))
  lazy val s_double: State[RNG,Double] = State(s => RNG.double(s))


  lazy val random_int: Int = s_nonNegativeInt.run(rng1)._1
  lazy val nonNegativeInt: Int = s_nonNegativeInt.run(rng1)._1
  lazy val double: Double = s_nonNegativeInt.run(rng1)._1

  import Gen.state2stream

  // Exercise 4
  def randomDoubles (seed: RNG): Stream[Double] =
    Stream.cons(RNG.double(seed)._1, randomDoubles(RNG.double(seed)._2))





  lazy val someRandomDoubles: List[Double] = state2stream(s_double)(rng1)
    .take(1000).toList
  lazy val moreRandomDoubles: List[Double] = {
    val s: RNG = Simple(43)
    state2stream(s_double)(s).take(1000).toList
  }

  // Exercise 5
  def impureRandomDoubles: Stream[Double] = randomDoubles(Simple(System
    .currentTimeMillis().toInt))


  lazy val impureDoubles1: Stream[Double] = impureRandomDoubles
  lazy val impureDoubles2: Stream[Double] = impureRandomDoubles

}

// A generator will use a random number generator RNG in its state, to create
// random instances (but perhaps also some other staff)
case class Gen[A] (sample: State[RNG,A]) {

  def listofGen (n: Int): List[Gen[A]] =
    List.fill(n)(this)

  // Let's convert generator to streams of generators
  def toStream (seed: Long): Stream[A] =
    Gen.state2stream (this.sample) (RNG.Simple (seed))

  def toStream (rng: RNG): Stream[A] =
    Gen.state2stream (this.sample) (rng)

  // Exercise 8

  def listOfN (n: Int): Gen[List[A]] = {
    val l = List.fill(n)(this)
    val s = State.sequence(l.map(x => x.sample))
    Gen(s)
  }

  // Exercise 9

  def flatMap[B] (f: A => Gen[B]): Gen[B] =
    Gen(this.sample.flatMap(x => f(x).sample))

  // It would be convenient to also have map  (uses flatMap)

  def map[B] (f: A => B): Gen[B] = this.flatMap (a => Gen.unit[B] (f(a)))

  // Exercise 10

  def listOfN (size: Gen[Int]): Gen[List[A]] = {
    this.flatMap()
  }

  // Exercise 11

  def union (that: Gen[A]): Gen[A] =
    Gen[A](this.sample.map2(that.sample)((x, y) =>
      if(boolean(Simple(42))._1) x
      else y))

  def union2 (that: Gen[A]): Gen[A] =
    that.flatMap(x => this.flatMap(y => Gen(State (s =>
      if (boolean(s)._1) (x,s)
      else (y,s)))))


  // Exercise 12 continues in the companion object (below)
}

object Gen extends App {

  // A convenience function to convert states (automata) to streams (traces)
  // It would be better to have it in State, but I am not controlling
  // State.scala.

  private[adpro]  def state2stream[A] (s :State[RNG,A]) (seed :RNG) :Stream[A] =
    s.run(seed) match { case (n,s1) => cons (n, state2stream (s) (s1)) }

  // A generator for Integer instances

  def anyInteger: Gen[Int] = Gen(State(_.nextInt))

  // Exercise 6

  def choose (start: Int, stopExclusive: Int): Gen[Int] =
    Gen(anyInteger.sample.map(x => math.abs(x % (stopExclusive-start)) +
      start))

  //print(choose(100, 105).toStream(13219).take(100).toList)
 // print(Gen(State(s => (12, s))).listOfN())
  // Exercise 7

  def unit[A] (a: =>A): Gen[A] = Gen(State(s => (a, s)))

 // print(unit(10).toStream(13421414).take(32).toList)

  def boolean: Gen[Boolean] = Gen(State(s => RNG.boolean(s)))

 // println(boolean.toStream(12414).take(10).toList)

  def double: Gen[Double] = Gen(State(s => RNG.double(s)))

 // println(double.toStream(12414).take(10).toList)

  print(double.listOfN(10).toStream(123243).take(10).toList)
  // (Exercise 8 is found in the Gen class above)

}

// This is the Prop type implemented in [Chiusano, Bjarnasson 2015]

object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  // the type of results returned by property testing

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified (
    failure: FailedCase,
    successes: SuccessCount
  ) extends Result {
      def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A] (as: Gen[A]) (f: A => Boolean): Prop = Prop {

    (n,rng) => as.toStream (rng).zip (Stream.from(0)).take(n).map {

      case (a,i) => try {
        if (f (a)) Passed else Falsified (a.toString, i)
      } catch { case e: Exception => Falsified (buildMsg(a, e), i) }

    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A] (s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

import Prop._

case class Prop (run: (TestCases,RNG)=>Result) {

  // (Exercise 12)

  def && (that: Prop): Prop = this.run()

  def || (that: Prop): Prop = ???

}

// vim:cc=80:foldmethod=indent:nofoldenable
