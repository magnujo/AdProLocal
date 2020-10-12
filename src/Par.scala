// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group name: FSF
//
// AUTHOR1: Joakim Hey Hinnerskov
// TIME1: 8-10
//
// AUTHOR2: Ask Harup Sejsbo
// TIME2: 8-10
//
// AUTHOR 3: Magnus Johansen
// TIME3: 8-10

package adpro

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors, TimeUnit}

import scala.language.implicitConversions
import scala.io.Source

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

object Par {

  trait Future[+A] {
    private[adpro] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new java.util.concurrent.atomic.AtomicReference[A] 
    val latch = new CountDownLatch(1) 
    p(es) { a => ref.set(a); latch.countDown } 
    latch.await 
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  /** A non-strict version of `unit` */
  def delay[A](a: => A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
    def apply(k: A => Unit) = f(k)
  }

  def eval (es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  def map2[A,B,C] (p: Par[A], p2: Par[B]) (f: (A,B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A,B]](es) {
          case Left(a) =>
            if (br.isDefined) eval(es)(cb(f(a,br.get)))
            else ar = Some(a)
          case Right(b) =>
            if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
            else br = Some(b)
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }
  
  // map is shown in the blocking part of the book (should still work but
  // probably uses one thread more than the version  below

  // def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
  //   map2 (pa,unit (())) ((a,_) => f(a))
  
  // This is the version of map2 specialized for nonblocking (Section 7.4.4
  // version)

  def map[A,B] (p: Par[A]) (f: A => B): Par[B] =
    es => new Future[B] {
      def apply (cb: B => Unit): Unit =
        p (es) ( a => eval (es) { cb (f (a)) } )
    }

  // Exercise 1
  //
  // We believe this has to do with achieving actual parralization in practice
  // If the argument was call by value, then the a would just be evaluated to start with, and any other units to be executed would wait for the execution of the first unit
  
  // Exercise 2 (CB7.4)
  def asyncF[A,B] (f: A => B) : A => Par[B] = s => {
    lazyUnit(f(s))
  }


  // Exercise 3
  // Assuming this is the map implementation in question
  // def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
  //   map2 (pa,unit (())) ((a,_) => f(a))

  //First we would test whether the structure of the input is preserved.
  //If a Par[List[int]] is passed to the map function, then the same type should be returned.
  //Also, we could execute f() on elements of the input list, and see if the output matches the elements in the
  //output list

  // Exercise 4 (CB7.5)
  def sequence[A] (ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => lazyUnit(List())
    case h :: t => map2(h, sequence(t))((a,b) => a :: b )
  }

  // Exercise 5 (CB7.6)
  // this is shown in the book:
  def parMap[A,B] (as: List[A]) (f: A => B): Par[List[B]] =
    sequence (as.map (asyncF (f)))

  def parFilter[A] (as: List[A]) (f: A => Boolean): Par[List[A]] ={
    map(parMap(as)(x => if (f(x)) Some(x) else None))(x => x.flatten) 
  }

  def parForall[A] (as: List[A]) (f: A => Boolean): Par[Boolean] ={
    map(parMap(as)(x => if (f(x)) Some(x) else None))(x => x.flatten)
  }
  
  // Exercise 6
  def map3[A,B,C,D] (pa :Par[A], pb: Par[B], pc: Par[C]) (f: (A,B,C) => D): Par[D] = {
    val first = map2(pa, pb)((a,b) => c => f(a,b,c))
    map2(first, pc)((f1,c) => f1(c))
  }
  // shown in the book (adjusted for the non-blocking version)

   def equal[A] (e: ExecutorService) (p: Par[A], p2: Par[A]): Boolean = 
     p(e) == p2(e)

  // Exercise 7 (CB7.11)
  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]): Par[A] =
  es => { 
    val N = run(es)(n)
    choices(N)(es)
  }

  def choice[A] (n: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] = {
    val N = map (n)(x => if(x) 0 else 1)
    val L = List[Par[A]](t, f)
    choiceN(N)(L)
  }

  // Exercise 8 (CB7.13)
 // note to self: chooser evaluates on one par, and passes it to another
  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] = es => {
    val a = run(es)(pa)
    val c = choices(a) 
    c(es)
  }

  // not entirely sure this is completely correct
  def choice1[A] (n: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(n)((bool) => if (bool) t else f)

  def choiceN1[A] (n: Par[Int]) (choices: List[Par[A]]): Par[A] = 
    chooser(n)((n) => choices(n))



  // Exercise 9 (CB7.14)
  def flatMap[A] (a: Par[A]) (f :A => Par[A]): Par[A] = es => {
    join(map(a)(f))(es)

  }

  def join[A] (a : Par[Par[A]]) :Par[A] = es => {
    run(es)(a)(es)
  }

  // Exercise 10
  implicit class Par2[A](p: Par[A]) {
    // Now it would be possible to make a val p: Par[Int] and say p.map(x => x + 1) etc.

    def map[B] (f: A => B): Par[B] = {
      val b: Par[B] = Par.map(p) (f)
      b
    }
    def map2[B,C] (p2: Par[B]) (f: (A,B) => C) : Par[C] = {
      val c: Par[C] = Par.map2(p, p2) (f)
      c
    }

    def chooser[B] (choices: A => Par[B]) : Par[B] = {
        val b = Par.chooser(p) (choices)
        b
      }
  }

  // Exercise 11
  def wget (uris: String*): List[String] = {
    val ES: ExecutorService = Executors.newFixedThreadPool(4)
    val a = parMap(uris.toList) (x => Source.fromURL(x).mkString)
    run (ES) (a)
  }
}

// This is in parellel because parMap runs async