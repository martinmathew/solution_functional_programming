package com.martin.chapter3

import com.martin.chapter3.Exercise_1.{Cons, List, Nil}

/**
  * Created by Martin on 2/5/2017.
  */
object Exercise10 {

  def main(args: Array[String]): Unit = {
    println(reverse(List(3,2,1)))
  }

  /**
    * EXERCISE 10: foldRight is not tail-recursive and will StackOverflow
for large lists. Convince yourself that this is the case, then write another general
list-recursion function, foldLeft that is tail-recursive, using the techniques we
discussed in the previous chapter.
    *
    * @return
    */

  def foldLeft[a, b](_xs: List[a], z: b)(f: (b, a) => b): b =
  _xs match {
    case Nil =>  z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }



  def foldRight[A,B](ints:List[A],num:B)(fun:(B,A)=>B):B ={
    ints match {
      case Nil => num
      case Cons(x,xs) => fun(foldRight(xs,num)(fun),x)
    }
  }

 /** EXERCISE 11: Write sum, product, and a function to compute the length of
    a list using foldLeft.**/

  def sumUsingFoldList(list: List[Int]):Int = {
    foldLeft(list,0)((x,y) => x + y)
  }


  def leftSum[Int](_xs: List[scala.Int
    ]): scala.Int =
    foldLeft(_xs, 0)((x, y) => x + y)


  def leftProd[Int](_xs:List[scala.Int]):scala.Int = {
    foldLeft(_xs,1)(_*_)
  }

  def length(xs:List[Int]):Int = {
    foldLeft(xs,0)((x,y)=> x+1)
  }


  /**
    * EXERCISE 12: Write a function that returns the reverse of a list (so given
List(1,2,3) it returns List(3,2,1)). See if you can write it using a fold.
    * @param xs
    */
  def reverse(xs:List[Int]) ={
    foldLeft(xs,Nil:List[Int])((ys:List[Int],x:Int) => Cons(x,ys))
  }

}
