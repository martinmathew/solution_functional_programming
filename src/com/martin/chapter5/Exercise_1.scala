package com.martin.chapter5

import com.martin.chapter4.Exercise4._
import com.sun.xml.internal.ws.resources.AddressingMessages
/**
  * Created by Martin on 3/29/2017.
  */
sealed trait Stream[+A] {
  def headOption[A] = this match {
    case Empty => None
    case Cons(h, t) => Option(h())
  }





  def size[A](st:Stream[A]) :Int = {
    this match {
      case Cons(h,t) => size(t())+1
      case Stream.empty => 0
    }
  }
  /**
    * EXERCISE 5.1
    * Write a function to convert a Stream to a List, which will force its evaluation and let
    * you look at it in the REPL. You can convert to the regular List type in the standard
    *library. You can place this and other functions that operate on a Stream inside the
    * Stream trait.
    *
    * @return
    */
  def toList: List[A] = {
    this match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }

  }

  /**
    * EXERCISE 5.2
    * Write the function take(n) for returning the first n elements of a Stream, and
    * drop(n) for skipping the first n elements of a Stream.
    *
    * @param n
    * @return
    */
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Empty)
      case _ => Empty
    }
  }

  /**
    * EXERCISE 5.3
    * Write the function takeWhile for returning all starting elements of a Stream that
    * match the given predicate.
    *
    * @param p
    * @return
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  /**
    * EXERCISE 5.4
    * Implement forAll, which checks that all elements in the Stream match a given predicate.
    * Your implementation should terminate the traversal as soon as it encounters a
    * nonmatching value.
    *
    * @param p
    * @return
    */
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((h, r) => p(h) && r)
  }

  /**
    * EXERCISE 5.5
    * Use foldRight to implement takeWhile
    *
    * @param p
    * @return
    */
  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Stream.empty)((h, r) => {
      if (p(h)) Stream.cons(h, r) else Stream.empty
    })
  }

  /**
    * EXERCISE 5.6
    * Hard: Implement headOption using foldRight
    *
    * @return
    */
  def headOption: Option[A] = {
    foldRight[Option[A]](None)((h, r) => Some(h))
  }

  /**
    * EXERCISE 5.7
    * Implement map, filter, append, and flatMap using foldRight. The append method
    * should be non-strict in its argument.
    *
    * @param f
    * @tparam B
    * @return
    */
  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((h, r) => Stream.cons(f(h), r))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, r) => if (f(h)) Stream.cons(h, r) else r)
  }

  def append[B >: A](s: Stream[B]): Stream[B] = {
    foldRight(s)((h, r) => Stream.cons(h, r))
  }

  /**
    * EXERCISE 5.8
    * Generalize ones slightly to the function constant, which returns an infinite Stream of
    * a given value.
    * def constant[A](a: A): Stream[A]
    *
    * @param a
    * @return
    */
  def constant(a: A): Stream[A] = {
    val as = Stream.cons(a, as)
    as
  }

  /**
    * EXERCISE 5.9
    * Write a function that generates an infinite stream of integers, starting from n, then n
    * + 1, n + 2, and so on.7
    *
    * @param n
    * @return
    */
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  /**
    * EXERCISE 5.10
    * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1,
    * 2, 3, 5, 8, and so on
    *
    * @return
    */
  def fib(): Stream[Int] = {
    def fib(first: Int, second: Int): Stream[Int] = {
      Stream.cons(first, fib(second, first + second))
    }

    fib(0, 1)
  }

  /**
    * // 5.11 Write a more general stream-building function called unfold.
    * // It takes an initial state, and a function for producing both the next state
    * // and the next value in the generated stream.
    *
    * @param z
    * @param f
    * @tparam A
    * @tparam S
    * @return
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Stream.empty
    }

  }

  // 5.12 Write fibs, from, constant, and ones in terms of unfold.
  def constantWithUnfold[A](a: A): Stream[A] = {
    unfold(a)(_ => Some((a, a)))
  }

  def fromWithUnfold[Int](n: Int): Stream[Int] = {
    unfold(n)(_ => Some((n + 1, n + 1)))


    def fibWithUnfold[Int] = {
      unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))
    }
  }

  /**
    * EXERCISE 5.13
Use unfold to implement map , take , takeWhile , zipWith (as in chapter 3), and
zipAll . The zipAll function should continue the traversal as long as either stream
has more elements—it uses Option to indicate whether each stream has been
exhausted.
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def mapWithUnfold[A, B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h), t())
      case _ => None
    }
  }

  def takeWithInfold(n: Int): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if n >= 1 => Some(h(), takeWithInfold(n - 1))
      case _ => None
    }

  }

  def takeWhileWithUnfold(f: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if f(h) => Some(h(), takeWhileWithUnfold(f))
      case _ => None
    }
  }


  def zipWithUnfold[A,B,C](stream1: Stream[A], stream2: Stream[B])(f:(A,B)=>C): Stream[C] = {
    unfold((stream1, stream2)) {
      case (Cons(h, t), Cons(p, y)) => Some(f(h(), p()), (t(), y()))

    }


    def toList[A]():List[A] = {
      this match {
        case Cons(x,t) => List(x()):::t().toList
        case _ => List.empty

      }
    }



  }

  /** Exercise 5.14
  // Hard: Implement startsWith using functions you’ve written. It should check
  // if one Stream is a prefix of another. For instance,
  // Stream(1,2,3) startsWith Stream(1,2) would be true.
    *
    * @param s
    * @tparam A
    * @return
    */
  def startsWith[A](s:Stream[A]):Boolean = {
  zipWithUnfold(this,s)
    takeWhileWithUnfold{
      case ((Cons(h,t),Cons(k,l))) if(h()==k()) => true
        case((Cons(h,k),Stream.empty)) => true
      case ((Cons(h,k),Cons(l,y))) => false
    }
  .forAll{
    case ((Cons(a,t),Cons(y,b))) if(a()==y()) => true
    case (Stream.empty,Stream.empty) => true
    case (_,_)=>false
  }
}


  def tail[A]():Stream[Stream[A]] = {
    unfold(this){
      case Cons(s,t)=> Some((this,t()))
      case Empty => None
    }
  }



  def scanRight[A,B](b:B)(f:(A,B) => B)= {
    this.tail().map(st => st.foldRight(b)(f)).toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def scanRight[A,B](b:B)(f:(A,B) => B)= {
    foldRight((b,Stream(b)))((a,b) => {
      lazy val lb = b
      val x = f(a,lb._1)
      (x,Stream.cons(x,lb._2))
    })._2
  }



}


case object Empty extends Stream[Nothing]
case class Cons[+A](h:() => A,t:() => Stream[A]) extends Stream[A]


object Stream {

  def cons[A](hd: => A,tl: => Stream[A]):Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head,() => tail)
  }


  def fib():Stream[Int]={
    def fib1(f:Int,sec:Int):Stream[Int] = {
      Stream.cons(f,fib1(sec,f+sec))
    }
    fib1(0,1)
  }
  def main(args:Array[String]):Unit = {
    fib()
  }
  def empty[A]:Stream[A] = Empty

  def apply[A](as:A*):Stream[A] = {
    if(as.isEmpty) empty else cons(as.head,apply(as.tail:_*))
  }


}




