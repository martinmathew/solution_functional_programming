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
        list
      }
    else {
      list match {
        case Exercise_1.Nil => Exercise_1.Nil
        case Cons(x, xs) => drop(xs,count-1)
      }
    }
  }


}
