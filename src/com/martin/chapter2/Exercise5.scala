package com.martin.chapter2

/**
  * Created by Martin on 1/29/2017.
  *
  * EXERCISE 5 (optional): Implement uncurry, which reverses the
transformation of curry. Note that since => associates to the right, A => (B
=> C) can be written as A => B => C.
  */
object Exercise5 {

  def main(args: Array[String]): Unit = {
    println(uncurry((x:Int) => (y:String) => x+y)(1,"Martin"))
  }

  def uncurry[A,B,C](f:A => B => C):(A,B) => C = {
    def f1(a:A,b:B) : C = f(a)(b)
    f1
  }
}
