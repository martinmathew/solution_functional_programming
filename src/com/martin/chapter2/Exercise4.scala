package com.martin.chapter2

/**
  * Created by Martin on 1/29/2017.
  *
  *
  * EXERCISE 4 (hard): Let's look at another example, currying, which converts a
function of N arguments into a function of one argument that returns another
function as its result.11 Here again, there is only one implementation that
typechecks.
  */
object Exercise4 {

  def curry[A,B,C](f:(A,B)=>C) : A => (B => C) = {
    def f1(a:A) : B => C = {
      def f2(b:B) : C = {
        f(a,b)
      }
    f2
    }
    f1
  }


  def main(args: Array[String]): Unit = {
   println( curry((x:Int,y:String) => x+y)(1)("dfg"))
  }


}
