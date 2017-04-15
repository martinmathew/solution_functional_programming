package com.martin.chapter2

/**
  * Created by Martin on 1/29/2017.
  */
object Exercise2 {


  def main(args: Array[String]): Unit = {
    val arr =  Array(2,3,4,5,9)
    println(isSorted(arr,(x:Int,y:Int) => x < y))
  }


  def isSorted[A](arr:Array[A],gt:(A,A) => Boolean) : Boolean ={
    def check(index:Int):Boolean = {
      if(index == arr.length-1)
        {
           true
        }
      else if (!gt(arr(index),arr(index+1)))
        {
          false
        }
       else {
          check(index+1)

        }
    }
    check(0)
  }

}
