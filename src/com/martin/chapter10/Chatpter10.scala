package com.martin.chapter10

object Chatpter10 {


  trait Monoid[A]{
    def op(inp:A,inp1:A):A

    def zero:A
  }


  /**
    *
    * Give Monoid instances for integer addition and multiplication as well as the Boolean
operators.
val intAddition: Monoid[Int]
val intMultiplication: Monoid[Int]
val booleanOr: Monoid[Boolean]
val booleanAnd: Monoid[Boolean]
    *
    *
    *
    */
  val intAdditionMonoid = new Monoid[Int] {
    override def op(inp: Int, inp1: Int) = inp + inp1

    override def zero = 0
  }

  val intMultiplicationMonoid = new Monoid[Int] {
    override def op(inp: Int, inp1: Int) = inp * inp1

    override def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(inp: Boolean, inp1: Boolean) = inp || inp1

    override def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(inp: Boolean, inp1: Boolean) = inp && inp1

    override def zero = true
  }

}
