package com.martin.chapter3

import com.martin.chapter3.Exercise_1.Cons

/**
  * Created by Martin on 2/1/2017.
  */
object Exercise3 {
  def main(args: Array[String]): Unit = {
    val inp = Cons(1,Cons(2,Cons(3,Exercise_1.Nil)))
    println(drop(inp,1))
  }

 /** EXERCISE 2: Implement the function tail for "removing" the first element
    of a List. Notice the function takes constant time. What are different choices you
  could make in your implementation if the List is Nil? We will return to this
  question in the next chapter.**/

  def tail(list:Exercise_1.List[Int]): Exercise_1.List[Int]={
    list match
      {
      case Exercise_1.Nil => Exercise_1.Nil
      case Cons(x,xs) => xs
    }
  }

  /**
    * EXERCISE 3: Generalize tail to the function drop, which removes the first
n elements from a list.
    * @param list
    * @param count
    * @return
    */

  def drop(list:Exercise_1.List[Int],count:Int):Exercise_1.List[Int]={
    if(count == 0)
      {
        Vector
        list
      }
    else {
      list match {
        case Exercise_1.Nil => Exercise_1.Nil
        case Cons(x, xs) => drop(xs,count-1)
      }
    }
  }

 /** EXERCISE 4: Implement dropWhile,10 which removes elements from the
    List prefix as long as they match a predicate. Again, notice these functions take
    time proportional only to the number of elements being dropped—we do not need
    to make a copy of the entire List.*/
  def dropWhile(list:Exercise_1.List[Int],fun:(Int) => Boolean): Exercise_1.List[Int] = {
    list match {
      case Exercise_1.Nil => Exercise_1.Nil
      case Cons(x,xs) => fun(x) match {
        case false => list
        case true => dropWhile(xs,fun)
      }
    }
  }

  /**
    * EXERCISE 5: Using the same idea, implement the function setHead for
replacing the first element of a List with a different value.
    * @param list
    * @param head
    * @return
    */

  def setHead(list:Exercise_1.List[Int],head:Int) :Exercise_1.List[Int] = {
    list match {
      case Exercise_1.Nil => Cons(head,Exercise_1.Nil)
      case Cons(x,xs) => Cons(head,xs)
    }
  }

  /**
    * EXERCISE 6: Not everything works out so nicely. Implement a function,
init, which returns a List consisting of all but the last element of a List. So,
given List(1,2,3,4), init will return List(1,2,3).
    */


  def init[A](l:Exercise_1.List[A]): Exercise_1.List[A] = {
  l match {
    case Exercise_1.Nil => Exercise_1.Nil
    case Cons(x,Exercise_1.Nil) => Exercise_1.Nil
    case Cons(x,xs) => Cons(x,init(xs))
  }

  }



  def init[A](list:scala.List[A]):scala.List[A] = {

    list match {
      case Nil => Nil
      case s::Nil => Nil
      case s::xs => s :: init(xs)
    }
  }








}
