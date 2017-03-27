package com.martin.chapter4



import scala.util.Try

/**
  * Created by Martin on 3/5/2017.
  */






class Exercise4 {

  /**
    * EXERCISE 4.1
Implement all of the preceding functions on Option. As you implement each function,
try to think about what it means and in what situations you’d use it. We’ll explore when
to use each of these functions next. Here are a few hints for solving this exercise:
 It’s fine to use pattern matching, though you should be able to implement all
the functions besides map and getOrElse without resorting to pattern matching.
 For map and flatMap, the type signature should be enough to determine the
implementation.
 getOrElse returns the result inside the Some case of the Option, or if the Option
is None, returns the given default value.
 orElse returns the first Option if it’s defined; otherwise, it returns the second
Option.
    * @tparam A
    */
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
      this match {
        case None => None
        case Some(x) => Some(f(x))
      }
    }
    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f) getOrElse None
    }
    def getOrElse[B >: A](fun: => B): B =
    this match {
      case None => fun
      case Some(a) => a
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      map(Some(_)) getOrElse None
    }
    def filter(f: A => Boolean): Option[A] = {
      flatMap((a:A) => if (f(a)) Some(a) else None)
    }
  }


  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {

    def mean(xs:Seq[Double]):Option[Double] = {
      if(xs.isEmpty) None else Some(xs.sum)
    }

    /**
      * EXERCISE 4.2
Implement the variance function in terms of flatMap. If the mean of a sequence is m,
the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
def variance(xs: Seq[Double]): Option[Double]
      * @param xs
      * @return
      */
    def variance(xs:Seq[Double]):Option[Double] = {
      mean(xs).flatMap(me => mean(xs.map(x => Math.pow(me-x,2))))

    }

    /**
      * EXERCISE 4.3
Write a generic function map2 that combines two Option values using a binary function.
If either Option value is None, then the return value is too. Here is its signature:
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]
      * @param a
      * @param b
      * @param f
      * @tparam A
      * @tparam B
      * @tparam C
      * @return
      */


    def map2[A,B,C](a:Option[A],b:Option[B])(f:(A,B) => C):Option[C] = {
      a.flatMap(a1 => b.map(b1 => f(a1,b1)))
    }

    def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

    def parseInsuranceRateQuote(
                                 age: String,
                                 numberOfSpeedingTickets: String): Option[Double] = {
      val optAge: Option[Int] = Try(age.toInt)
      val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)

     // lift(math.abs): Option[Double] => Option[Double]
     // math.abs: Double => Double

      Try(insuranceRateQuote(optAge, optTickets))
    }


    def Try[A](a: => A): Option[A] =
      try Some(a)
      catch { case e: Exception => None }


    /**
      * Top secret formula for computing an annual car
      * insurance premium from two key factors.
      */
    def insuranceRateQuote(age: Option[Int], numberOfSpeedingTickets: Option[Int]): Double = { 9.0d}

    /**
      * EXERCISE 4.4
Write a function sequence that combines a list of Options into one Option containing
a list of all the Some values in the original list. If the original list contains None even
once, the result of the function should be None; otherwise the result should be Some
with a list of all the values. Here is its signature:3
      * @param a
      * @tparam A
      * @return
      */

    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      a.foldRight[Option[List[A]]](Some(Nil))((oa,ob) => map2(oa,ob)(_::_))
    }

    def parseInts(a:List[String]):Option[List[Int]] = {
      sequence(a map (i => Try(i.toInt)))
    }

    // Exercise 4.5
    // Implement this function. It’s straightforward to do using map and sequence,
    // but try for a more efficient implementation that only looks at the list once.
    // In fact, implement sequence in terms of traverse.
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))

    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
      traverse(a)(x => x)



  }


  /**
    * EXERCISE 4.6
Implement versions of map, flatMap, orElse, and map2 on Either that operate on the
Right value.
    * @tparam E
    * @tparam A
    */

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Left(e) => Left(e)
        case Right(e) => Right(f(e))
      }
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }
    }


    def flatMapComp[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {

        case Left(_) => b
        case Right(a) => Right(a)
      }
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
    Either[EE, C] = this.flatMap(a => b.map(bb => f(a,bb)))

    /**
      * EXERCISE 4.8
In this implementation, map2 is only able to report one error, even if both the name
and the age are invalid. What would you need to change in order to report both errors?
Would you change map2 or the signature of mkPerson? Or could you create a new data
type that captures this requirement better than Either does, with some additional
structure? How would orElse, traverse, and sequence behave differently for that
data type?
      * @param that
      * @param f
      * @tparam EE
      * @tparam B
      * @tparam C
      * @return
      */
    def map2Comp[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C):
    Either[EE, C] = (this,that)
      match {
      case (Left(a),Left(e)) => CompositeLeft(List(a,e))
      case (Left(a),Right(e)) => Left(a)
      case (Right(a),Left(e)) => Left(e)
      case (Right(a),Right(b)) => Right(f(a,b))
      }

  }


  case class Right[+A](a:A) extends Either[Nothing,A]

  case class Left[+E](e:E) extends Either[E,Nothing]

  case class CompositeLeft[+E](l:List[E]) extends Left[E,Nothing]
  // Exercise 4.7
  // Implement sequence and traverse for Either. These should return the first
  // error that’s encountered, if there is one.

  def sequence[E,A](es:List[Either[E,A]]):Either[E,List[A]]={
    es.foldRight[Either[E,List[A]]](Right(Nil))((e:Either[E,A],acc:Either[E,List[A]]) => e.map2(acc)(_::_))
  }

  def traverse[E,A,B](as:List[A])(f:A => Either[E,B]):Either[E,List[B]] = {
    as.foldRight[Either[E,List[B]]](Right(Nil))((a:A,acc:Either[E,List[B]]) => f(a).map2(acc)(_::_))
  }


  def traverseComp[E,A,B](as:List[A])(f:A => Either[E,B]):Either[E,List[B]] = {
    as.foldRight[Either[E,List[B]]](Right(Nil))((a:A,acc:Either[E,List[B]]) => f(a).map2(acc)(_::_))
  }

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name:String,age:Int):Either[String,Person] = {
    mkName(name).map2(mkAge(age))(Person(_,_))
  }


}

object Exercise4 {
import Exercise4._
  def main(args: Array[String]): Unit = {
    val ex = new Exercise4
    val eitherList = List(ex.Left[String]("ty"),ex.Right[String]("rt"))
    val res = ex.sequence(eitherList)
    println(res)

  }





}
