package com.martin.chapter6

import java.lang.Math.RandomNumberGeneratorHolder

/**
  * Created by martin on 4/29/17.
  */
object Exercise6 {


  trait RNG{
    def nextInt:(Int,RNG)
  }


  case class SimpleRNG(seed:Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66Dl+0xBL)&0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n,nextRNG)
    }



  }

  /**
    * // Exercise 6.1
  // Write a function that uses RNG.nextInt to generate a random integer between
  // 0 and Int.maxValue (inclusive). Make sure to handle the corner case when
  // nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart.
    *
    *
    * @param rng
    * @return
    */
  def nonNegativeInt(rng:RNG):(Int,RNG) = {
    val (num,rg)=rng.nextInt
    (Math.abs(num),rg)
  }

  /**
    * EXERCISE 6.2
Write a function to generate a Double between 0 and 1 , not including 1 . Note: You can
use Int.MaxValue to obtain the maximum positive integer value, and you can use
x.toDouble to convert an x: Int to a Double .
    * @param rng
    * @return
    */
  def double(rng:RNG):(Double,RNG) = {
    val tuple = rng.nextInt
    (tuple._1.toDouble/Double.MaxValue,tuple._2)
  }

  /**Ex 6.3
    * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
(Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
already written.
    * @param rng
    * @return
    */
  def intDouble(rng:RNG):((Int,Double),RNG) = {
    val tupleInt = rng.nextInt
    val tupleDb = double(rng)
    ((tupleInt._1,tupleDb._1),rng)
  }

  /**Ex 6.4
    * Write a function to generate a list of random integers.
def ints(count: Int)(rng: RNG): (List[Int], RNG)
    * @param count
    * @param rng
    * @return
    */
  def ints(count:Int)(rng:RNG):(List[Int],RNG) = {
    if(count == 0)
      {
        ( List.empty[Int],rng)
      }else{
      val tuple = rng.nextInt
      val tuple1 = ints(count-1)(tuple._2)
      (tuple._1 :: tuple1._1 ,tuple._2)
    }
  }


  type Rand[+A] = RNG => (A,RNG)

  val int:Rand[Int] = _.nextInt

  def unit[A](a:A):Rand[A] = rng => (a,rng)



  def nonNegativeEven():Rand[Int]={
    map(nonNegativeInt)(i => i-i%2)
  }

  /**
    * EXERCISE 6.5
Use map to reimplement double in a more elegant way. See exercise 6.2.
    * @param rng
    * @return
    */
  def betterDouble(rng:RNG):Rand[Double] = {
    map((rng=>(rng.nextInt._1,rng.nextInt._2)))(i => i.toDouble)
  }


  /**
    * EXERCISE 6.6
Write the implementation of map2 based on the following signature. This function
takes two actions, ra and rb, and a function f for combining their results, and returns
a new action that combines them:
    * @param ra
    * @param rb
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def map2[A,B,C](ra:Rand[A],rb:Rand[B])(f:(A,B)=>C):Rand[C] = rng => {
    val (a,arng) = ra(rng)
    val (b,brng) = rb(arng)
    (f(a,b),brng)
  }



  def map[A,B](s:Rand[A])(f:A=>B):Rand[B] = rng => {
    val (a,rng2) = s(rng)
    (f(a),rng2)
  }

  /**
    * EXERCISE 6.7
Hard: If you can combine two RNG transitions, you should be able to combine a whole
list of them. Implement sequence for combining a List of transitions into a single
transition. Use it to reimplement the ints function you wrote before. For the latter,
Licensed to Dustin Withers <dustin@7sudos.com>
86 CHAPTER 6 Purely functional state
you can use the standard library function List.fill(n)(x) to make a list with x
repeated n times.
    * @param fs
    * @tparam A
    * @return
    */
  def sequence[A](fs:List[Rand[A]]):Rand[List[A]] = rng => {
    val res = (fs.map(rnd => {val tuple = rnd(rng)
      (tuple._1,tuple._2)
    }))
    ( res.map(_._1),res(res.size-1)._2)
  }


  def sequence1[A](fs:List[Rand[A]]):Rand[List[A]] = {
    fs.foldLeft(unit(Nil: List[A]))(( acc,el) => map2(el, acc)(_ :: _))
  }

  /**
    *EXERCISE 6.8
Implement flatMap , and then use it to implement nonNegativeLessThan .
def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B]
    * @param f
    * @param g
    * @tparam A
    * @tparam B
    * @return
    */
  def flatMap[A,B](f:Rand[A])(g:A=>Rand[B]):Rand[B] = rng => {
    val a1 = f(rng)
     g(a1._1)(a1._2)
  }

  def mapViaFlatmap[A,B](s:Rand[A])(f:A=>B):Rand[B] =  {
    flatMap(s)(a=>unit(f(a)))
  }
}
