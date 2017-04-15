package com.martin.chapter3

import scala.collection.immutable

/**
  * Created by Martin on 2/1/2017.
  */









object Exercise_1 {

  /**
    * Created by Martin on 1/31/2017.
    */
  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]


  def main(args: Array[String]): Unit = {

    val x11 = List(List(1, 2, 3, 4, 5),List(6,7,8,9),List(10,11,12))
    val x1 = List(1,2,3,4)
  // println(List.filter(x1)(x => x%2 == 0))
    val sup = immutable.List(1,2,3,4)
    val sub = immutable.List(2,9)
    println(List.subsequence(sup,sub))
  }

  object List {


    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }


    def product(ints: List[Int]): Int = ints match {
      case Nil => 1
      case Cons(0, _) => 0
      case Cons(x, xs) => x * product(xs)
    }


    def foldRightWithInvariant(ints: List[Int], num: Int)(invariantCheck: (Int) => Boolean)(fun: (Int, Int) => Int): Int = {
      ints match {
        case Nil => num
        case Cons(x, xs) => if (invariantCheck(x)) 0 else fun(x, foldRightWithInvariant(xs, num)(invariantCheck)(fun))
      }
    }

    def sumFold[A](ints: List[Int]): Int = {
      foldRightWithInvariant(ints, 0)((a: Int) => false)(_ + _)
    }

    /** EXERCISE 7: Can product implemented using foldRight immediately
      * halt the recursion and return 0.0 if it encounters a 0.0? Why or why not?
      * Consider how any short-circuiting might work if you call foldRight with a
      * large list. This is a deeper question that we'll return to a few chapters from now. **/
    def productFold[A](ints: List[Int], num: Int): Int = {
      foldRightWithInvariant(ints, 1)(_ == 0)((a: Int, b: Int) => a * b)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))


    def test = {
      val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }
      println(x)
    }


    def foldRight[A, B](ints: List[A], num: B)(fun: (B, A) => B): B = {
      ints match {
        case Nil => num
        case Cons(x, xs) => fun(foldRight(xs,num)(fun),x)
      }
    }


    def foldLeft[a, b](_xs: List[a], z: b)(f: (b, a) => b): b =
      _xs match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    /**
      * EXERCISE 9: Compute the length of a list using foldRight.
      *
      * @param list
      * @tparam Int
      * @return
      */
    def length[Int](list: List[scala.Int]): scala.Int = {
      foldRight(list, 0)((elem: scala.Int, index: scala.Int) => index + 1)
    }

    /**
      * EXERCISE 3.14
      * Implement append in terms of either foldLeft or foldRight*/
    def append[A](xs1: List[A], xs2: List[A]): List[A] = {
      xs1 match {
        case Nil => xs2
        case Cons(x, xs) => Cons(x, append(xs, xs2))

      }

    }

    /**
      *
      * EXERCISE 3.15
Hard: Write a function that concatenates a list of lists into a single list. Its runtime
should be linear in the total length of all lists. Try to use functions we have already
defined.
      * @param xs
      * @tparam A
      * @return
      */
    def concatenate[A](xs:List[List[A]]) : List[A] = {
  xs match {
    case Nil => Nil
    case Cons(x,Nil) => x
    case Cons(x,xs) => append(x,concatenate(xs))
  }
}

    def concatenate1[A](xs:List[List[A]]):List[A] = {
      foldLeft(xs,List[A]())((x,y) => append(x,y))
    }

    /**
      * EXERCISE 3.16
Write a function that transforms a list of integers by adding 1 to each element.
(Reminder: this should be a pure function that returns a new List!)
      * @param xs
      * @return
      */

    def add1(xs:List[Int]): List[Int] = {
      xs match
        {
        case Nil => Nil
        case Cons(x,ts) => Cons(x+1,add1(ts))
      }

    }


    def add1V2(xs:List[Int]): List[Int] = {
foldLeft(xs,Nil: List[Int])((x,y) => Cons(y+1,x))
    }


    /**
      * EXERCISE 3.17
Write a function that turns each value in a List[Double] into a String. You can use
the expression d.toString to convert some d: Double to a String.
      * @param xs
      * @return
      */
    def doubleToString(xs:List[Double]) : List[String] = {
foldLeft(xs,Nil:List[String])((x,y) => Cons(y.toString,x))
    }

    /**
      * EXERCISE 3.18
Write a function map that generalizes modifying each element in a list while maintaining
the structure of the list. Here is its signature:12
      * @param xs
      * @param f
      * @tparam A
      * @tparam B
      * @return
      */

    def map[A,B](xs:List[A])(f:A => B) :List[B] = {
      foldRight(xs,Nil:List[B])((x,y) => Cons(f(y),x))
    }

    /**
      * EXERCISE 3.19
Write a function filter that removes elements from a list unless they satisfy a given
predicate. Use it to remove all odd numbers from a List[Int].
      * @param xs
      * @param fun
      * @tparam A
      * @return
      */
    def filter[A](xs:List[A])(fun : (A) => Boolean): List[A] = {
      xs match
        {
        case Nil => Nil
        case Cons(x,ys) => if(fun(x)) Cons(x,filter(ys)(fun)) else filter(ys)(fun)
      }
    }

    /**
      * Write a function flatMap that works like map except that the function given will return
a list instead of a single result, and that list should be inserted into the final resulting
list. Here is its signature:
      * @param xs
      * @param fun
      * @tparam A
      * @tparam B
      * @return
      */

    def flatMap[A,B](xs:List[A])(fun:A => List[B]):List[B] = {
      foldLeft(xs,Nil:List[B])((x,y) => append(fun(y),x) )
    }

    /**
      * EXERCISE 3.21
Use flatMap to implement filter.
      * @param xs
      * @param fun
      * @tparam A
      * @return
      */

    def filterUsingFlat[A](xs:List[A])(fun:A => Boolean) : List[A]={
      flatMap(xs)((x) => if(fun(x)) List(x) else  Nil)
    }

    /**
      * EXERCISE 3.22
Write a function that accepts two lists and constructs a new list by adding corresponding
elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
      * @param xs
      * @param ys
      * @tparam Int
      * @return
      */

    def addList(xs:List[Int],ys:List[Int]): List[Int] ={
      (xs,ys) match{
        case (Nil,_) => ys
        case (_,Nil) => xs
        case (Cons(s:Int,xs1),Cons(t:Int,ys1)) => Cons(t+s,addList(xs1,ys1))
      }

    }

    /**
      * EXERCISE 3.23
Generalize the function you just wrote so that it’s not specific to integers or addition.
Name your generalized function zipWith.
      * @param xs
      * @param ys
      * @param fun
      * @tparam A
      * @tparam B
      * @return
      */
    def zipWidth[A,B](xs:List[A],ys:List[A])(fun:(A,A) => B):List[B] = {
      (xs,ys) match {
        case (Nil,_) => Nil
        case (_,Nil) => Nil
        case (Cons(s,st:List[A]),Cons(t,tt:List[A])) => Cons(fun(s,t),zipWidth(st,tt)(fun))
      }
    }

    /**
      * EXERCISE 3.24
Hard: As an example, implement hasSubsequence for checking whether a List contains
another List as a subsequence. For instance, List(1,2,3,4) would have
List(1,2), List(2,3), and List(4) as subsequences, among others. You may have
some difficulty finding a concise purely functional implementation that is also efficient.
That’s okay. Implement the function however comes most naturally. We’ll
return to this implementation in chapter 5 and hopefully improve on it. Note: Any
two values x and y can be compared for equality in Scala using the expression x == y.
      * @param sup
      * @param sub
      * @return
      */

    def subsequence(sup:immutable.List[Int],sub:immutable.List[Int]):Boolean = {
      (sup,sub) match {

        case (_,_root_.scala.Nil) => true
        case (_root_.scala.Nil,_) => false
        case (x::xs,y::ys) => (x == y && subsequence(xs,ys)) || subsequence(xs,sub)
      }
    }




  }}
