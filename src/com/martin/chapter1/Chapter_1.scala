package com.martin.chapter1

/**
  * Created by Martin on 1/22/2017.
  */
class Chapter_1 {



  def buyCoffee(flavour:Int) : (Coffee,Charge) = {
    val cup = new Coffee
    val charge = new Charge(788)
    (cup,charge)
  }



}


class CreditCard {

}

class Coffee{

}


class Charge(val amount:Int)
