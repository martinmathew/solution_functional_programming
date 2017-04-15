package com.martin.chapter2

/**
  * Created by Martin on 1/29/2017.
  */
object Exercise3 {


  def main(args: Array[String]): Unit = {
    println(partial1(1,(x:Int,y:String) => x + 2.0)("Martin"))
  }



  def partial1[A,B,C](a:A,f:(A,B) => C): B=>C ={
    (b:B) => f(a,b)
  }

}
