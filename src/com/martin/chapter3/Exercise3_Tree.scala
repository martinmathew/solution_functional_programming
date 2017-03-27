package com.martin.chapter3

/**
  * Created by Martin on 3/3/2017.
  */
 object Exercise3_Tree {

  sealed trait Tree[+A]

  case class Leaf[A](value:A) extends Tree[A]
  case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

  /**
    * EXERCISE 3.25
       Write a function size that counts the number of nodes (leaves and branches) in a tree.
    * @param node
    * @tparam A
    * @return
    */

  def count[A](node:Tree[A]):Int = {
    node match {
      case Leaf(_) => 1
      case Branch(left,right) => count(left)+count(right)+1
    }
  }

  /**
    * EXERCISE 3.26
Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
and y.)
    * @param node
    * @return
    */

  def getmax(node:Tree[Int]):Int = {
    node match {
      case Leaf(x) => x
      case Branch(left,right) => getmax(left) max(getmax(right))
    }
  }

  /**
    * EXERCISE 3.27
Write a function depth that returns the maximum path length from the root of a tree
to any leaf.
    * @param node
    * @tparam A
    * @return
    */
  def depth[A](node:Tree[A]):Int = {
    node match {
      case Leaf(_) => 1
      case Branch(left,right) => depth(left) max depth(right) +1
    }
  }

  def depthWithFold[A](node:Tree[A]) : Int = {
    fold(node,0)((x:Int,a:A) => 1)((x:Int,y:Int) => (x max y)+1)
  }

  /**
    * EXERCISE 3.28
Write a function map, analogous to the method of the same name on List, that modifies
each element in a tree with a given function.
    * @param node
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def map[A,B](node:Tree[A])(f:A => B):Tree[B] = {
    node match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left,right) => Branch(map(left)(f),map(right)(f))
    }
  }

  /**
    * EXERCISE 3.29
Generalize size, maximum, depth, and map, writing a new function fold that abstracts
over their similarities. Reimplement them in terms of this more general function. Can
you draw an analogy between this fold function and the left and right folds for List?
    * @param node
    * @param value
    * @param f
    * @param f1
    * @tparam A
    * @tparam B
    * @return
    */

  def fold[A,B](node:Tree[A],value:B)(f:(B,A) => B)(f1:(B,B) => B) : B ={
    node match {
      case Leaf(x) => f(value,x)
      case Branch(left,right) => f1(fold(left,value)(f)(f1) , fold(right,value)(f)(f1))
    }
  }


}
