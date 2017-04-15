package com.martin.chapter2

/**
  * Created by Martin on 1/29/2017.
  *
  * EXERCISE 5 (optional): Implement uncurry, which reverses the
transformation of curry. Note that since => associates to the right, A => (B
=> C) can be written as A => B => C.
  */
object Exercise6 {



  def compose[A,B,C](f:B => C,g:A => B): A=>C =
  {
  def f1(a:A) : C = f(g(a))
  f1
  }

}
