package com.martin.chapter7

import com.martin.chapter7.Chapter7.{Future, Par}

import scala.actors.threadpool.Executors
import scala.concurrent.Awaitable
import scala.concurrent.duration.TimeUnit
import scala.util.parsing.json.JSON

/**
  * Created by martin on 6/17/17.
  */






object Chapter7 {

  /**
    * EXERCISE 7.2
Before continuing, try to come up with representations for Par that make it possible
to implement the functions of our API .
    *
    *
    *
    * @tparam T
    *
    *
    */
type Par[T] = ExecutorService => Future[T]



  trait Future[T] {

    def get:T
    def get(timeout:Long,unit:TimeUnit):T
    def cancel(evenIfRunning:Boolean):Boolean
    def isDone:Boolean
    def isCancelled:Boolean
  }

  /**
    *EXERCISE 7.1
Par.map2 is a new higher-order function for combining the result of two parallel com-
putations. What is its signature? Give the most general signature possible (don’t
assume it works only for Int ).
    *
    *
    */



def run[T](ex:ExecutorService)(a:Par[T]):Future[T] = {
  a(ex)
}

}

object Par {


  def unit[A](a:A):Par[A] = (es:ExecutorService) => UnitFuture(a)


  private case class UnitFuture[A](get:A) extends Future[A]{
    /**
      *
      * @param timeout
      * @param unit
      * @return
      */
    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(evenIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = false

    override def isCancelled: Boolean = false
  }
  def map2[A,B,C](f:Par[A],g:Par[B])(fun:(A,B)=> C):Par[C] = {
    (es:ExecutorService) =>  {
      val af = f(es)
      val bf = g(es)
      UnitFuture(fun(af.get,bf.get))
    }
  }


  def sortList(numList:Par[List[Int]]):Par[List[Int]] = {
    map2(numList,unit())((a,_)=>a.sorted)
  }



  def map[A,B](inp:Par[A])(f:A => B):Par[B] = {
    map2(inp,unit())((a,_) => f(a))
  }


  def fork[A](a: => Par[A]):Par[A] = {
    es:ExecutorService => es.submit(new Callable[A] {
      def call = a(es).get
    })
  }


  def delay[A](fa: => Par[A]):Par[A] = es => fa(es)

  def last[A](list: List[A]): A =
    list.foldLeft[A](list.head)((_, c) => c)


  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 7.4 This API already enables aPar rich set of operations. Here’s aPar simple example: using lazyUnit,
  // write aPar function to convert any function A => B to one that evaluates its result asynchronously.
  def async[A,B](f:A => B):A => Par[B] = {
    a => {
      fork(unit(f(a)))
    }
  }

  // 7.5 Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
  ps.foldLeft(unit(List[A]())){ (acc,elem) => map2(elem, acc)(_ :: _)  }

  def parMap[A,B](ps:List[A])(f:A => B):Par[List[B]] = {
    val fbs : List[Par[B]]=ps.map(async(f(_)))
    sequence(fbs)
  }

  /**
    * EXERCISE 7.6
Implement parFilter , which filters elements of a list in parallel.
    */

  def parFilter[A](ps:List[A])(f:A => Boolean):Par[List[A]] = {
    sequence(ps.filter(f(_)).map(unit(_)))
  }

  /**
    * EXERCISE 7.7
Hard: Given map(y)(id) == y , it’s a free theorem that map(map(y)(g))(f) ==
map(y)(f compose g) . (This is sometimes called map fusion, and it can be used as an
optimization—rather than spawning a separate parallel computation to compute the
second mapping, we can fold it into the first mapping.) 13 Can you prove it? You may
want to read the paper “Theorems for Free!” (http://mng.bz/Z9f1) to better under-
stand the “trick” of free theorems.
    *
    */

  // map(map(y)(g))(f)  =  map(g(y)(f) // using Initial law and subsistute
  //map(f(g(y)) == using Initial and subsistute
  //map(y)(f(g)) hence pproved


  // Exercise 7.9
  // Show that any fixed size thread pool can be made to deadlock given
  // this implementation of fork
  // Given a thread pool with a fixed size of N
  // If we nest  N + 1 calls of fork, we will result in a deadlock
  def deadlock[A](threadPoolSize: Int, a: A): Par[A] = {
    if (threadPoolSize <= 1) {
      unit(a)                                                     //for each thread in the pool , we are adding two items in thread pool
    } else {
      fork(deadlock(threadPoolSize - 1, a))
    }
  }







}
// 7.3 Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.

case class Map2WithFuture[A,B,C](fa:Future[A],fb:Future[B])(F:(A,B) => C) extends Future[C] {
  override def get: C = ???

  def compute(timeout: Long,unit: TimeUnit): C = {
    val startTime = System.currentTimeMillis()
    val a = fa.get(timeout,unit)
    val change =  System.currentTimeMillis()-startTime
    val b = fb.get(timeout,unit)
    F(a,b)
  }

  override def get(timeout: Long, unit: TimeUnit): C = compute(timeout,unit)

  override def cancel(evenIfRunning: Boolean): Boolean = ???

  override def isDone: Boolean = ???

  override def isCancelled: Boolean = ???
}




trait Callable[A] { def call : A}

class ExecutorService {

  def submit[A](a:Callable[A]):Future[A] = {

  }

}


