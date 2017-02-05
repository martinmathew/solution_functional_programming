package com.martin.chapter3

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

    val x = List(1, 2, 3, 4, 5)
   println(List.productFold(x,1))
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


    def foldRight(ints:List[Int],num:Int)(invariantCheck : (Int) => Boolean)(fun:(Int,Int)=>Int):Int ={
      ints match {
        case Nil => num
        case Cons(x,xs) =>if(invariantCheck(x)) 0 else fun(x,foldRight(xs,num)(invariantCheck)(fun))
      }
    }

    def sumFold[A](ints:List[Int]) :Int = {
      foldRight(ints,0)((a:Int) => true)(_ + _)
    }

   /** EXERCISE 7: Can product implemented using foldRight immediately
      halt the recursion and return 0.0 if it encounters a 0.0? Why or why not?
      Consider how any short-circuiting might work if you call foldRight with a
    large list. This is a deeper question that we'll return to a few chapters from now.**/
    def productFold[A](ints:List[Int],num:Int):Int = {
      foldRight(ints,1)(_==0)((a:Int,b:Int) =>  a*b)
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




  }

}
