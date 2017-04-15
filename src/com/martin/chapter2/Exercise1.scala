package com.martin.chapter2

/**
  * Created by Martin on 1/28/2017.
  */
object Exercise1 {

  def main(args: Array[String]): Unit = {
    println(fib(-1))
    println(fib(0))
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
  }



  def fib(n:Int) :Int = {


    def fibonacci(prev:Int ,curr:Int ,count:Int):Int = {
      if(count<=0)
        {
          return prev+curr;
        }
      else
        {
          fibonacci(curr,prev+curr,count -1)
        }
    }

    fibonacci(0,1,n);
  }

}
