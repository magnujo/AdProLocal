import java.nio.file.FileAlreadyExistsException

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail: Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f: (A, =>B) => B): B = this match {
    case Empty => z
    case Cons (h,t) => f (h(), t().foldRight (z) (f))
    // Note 1. f can return without forcing the tail
    // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
    // if f requires to go deeply into the stream. So folds sometimes may be
    // less useful than in the strict case
  }

  // Note. foldLeft is eager; cannot be used to work with infinite streams. So
  // foldRight is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
    case Empty => z
    case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
    // Note 2. even if f does not force z, foldLeft will continue to recurse
  }

  def exists (p : A => Boolean) :Boolean = this match {
    case Empty => false
    case Cons (h,t) => p(h()) || t().exists (p)
    // Note 1. lazy; tail is never forced if satisfying element found this is
    // because || is non-strict
    // Note 2. this is also tail recursive (because of the special semantics
    // of ||)
  }


  // Exercise 2

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h()::t().toList
  }

  // Exercise 3

  def take2 (n: Int): Stream[A] = {
    def loop (l: Stream[A], n: Int, acc: Stream[A]): Stream[A] = l match {
      case Empty => acc
      case Cons(h, t) =>
        if (n==0) acc
        else loop(t(),n-1, cons[A](h(), acc))
    }

    loop(this,n, Empty)
  }

  def take (n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xs) => if (n == 0) Empty else cons(x(), xs().take(n-1))
  }

  def drop (n: Int): Stream[A] = {
    def loop (l: Stream[A], n:Int): Stream[A] = l match {
      case Empty => Empty
      case Cons(h, t) =>
        if(n==0) l
        else loop(t(), n-1)
    }
    loop(this, n)
  }

  // Exercise 4

  def takeWhile (p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xs) => if (!p(x())) Empty else cons(x(), xs().takeWhile(p))
  }


 /* def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match {
      case Cons(h,t) if f(h) => dropWhile(t,f)
      case _ => as
    }*/


  //Exercise 5

  def forAll (p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(x, xs)  => if (p(x())) xs().forAll(p) else false
  }


  //Exercise 6

  def takeWhile2 (p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty) ((a, b) => if (!p(a)) Empty else cons[A](a, b.takeWhile2(p)))

  //Exercise 7

  def headOption2: Option[A] =
    foldRight[Option[A]](None)((a,b)=>Some(a))

  //Exercise 8 The types of these functions are omitted as they are a part of the exercises

  def map[B]  (f: A => B): Stream[B] = this match {
    case Empty => Empty
    case Cons(x, xs) => cons(f(x()), xs().map(f))
  }

  def map2[B]  (f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((a,b)=> cons(f(a), b))

  def filter (p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xs) => if (p(x())) cons(x(), xs().filter(p)) else xs().filter(p)
  }

  def filter2 (p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a,b) => if(p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B] (f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty[B]) ((h,t) => f(h).append(t))

  //Exercise 09
  //Put your answer here:
  /*
  Its optimal for streams because it when the filter have "found" a value the headOption is applied on the temporary stream containing only one element,
  which is the head of the whole stream, if the filter function was executed on the whole stream.
  For list it would go through the whole list before it would apply the head function. This is inefficient
  because we dont need the whole list to know what the head is.

  For example if you call Stream(1,2,3,4).find(_==3) the trace would look like this:
  Stream(1,2,3,4).filter(_==3).headOption
  Stream(2,3,4).filter(_==).headOption
  Stream(3,4).filter(_==).headOption
  cons(3, Stream(4).filter(_==3)).headOption = Some(3)
   */

  //Exercise 10
  //Put your answer here:
  def fibs: Stream[Int] = {
    def go (a: Int, b: Int): Stream [Int] = {
      cons(a, go(b, a+b))
    }
    go(0, 1)
  }


  // Exercise 13

  def map_[B] (f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(x, xs) => Some(f(x()), xs())
    }

  def take_ (n: Int): Stream[A] =
    unfold(this) {
      case Empty => None
      case Cons(x, xs) => if (n==0) None else Some(x(), xs().take_(n-1))
    }

  def takeWhile_ (p: A => Boolean): Stream[A] =
    unfold(this){
      case Empty => None
      case Cons(x, xs) => if (!p(x())) None else Some(x(), xs().takeWhile_(p))
    }

  def zipWith_[B,C] (f: (A,B) => C) (a2: Stream[B]): Stream[C] =
    unfold((this, a2)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(x1, xs1), Cons(x2, xs2)) => Some(f(x1(),x2()), (xs1(), xs2()))
    }

}


case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A, t: () => Stream[A]) extends Stream[A]



object Stream extends App {


  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  // Note 1: ":_*" tells Scala to treat a list as multiple params
  // Note 2: pattern matching with :: does not seem to work with Seq, so we
  //         use a generic function API of Seq


  // Exercise 1

  def from (n: Int): Stream[Int] = cons (n, from(n+1))

  def to (n: Int): Stream[Int] = cons(n, to(n-1))

  val naturals: Stream[Int] = from(1)

  val empt = Empty
  val stream2: Stream[Int] = cons(1, cons(2, cons(3, Stream.empty) ) )


  print(naturals.map (_%2==0).zipWith_[Boolean,Boolean] (_||_) (naturals.map (_%2==1)).take(10).toList)


  //Exercise 11

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  // Exercise 12

  def fib2  = unfold((0,1)) { case (x, y) => Some(x, (y, x+y))}
  def from2 (n: Int) = unfold(n) (n => Some(n, n+1))

}


