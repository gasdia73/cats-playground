package org.cats.mytests

import cats.Eq
import cats.implicits.catsSyntaxEq

object Equiv extends App {
  final case class Cat(name: String, age: Int, color: String)

  implicit val dateEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      cat1.name === cat2.name &&
        cat1.age === cat2.age &&
        cat1.color === cat2.color
    }

  val cat1 = Cat("Garfield",
    38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  private val catequal: Boolean = cat1 === cat2
  println(s"comparing cats: ${catequal}")

  private val catdifferent: Boolean = cat1 =!= cat2
  println(s"are cats different?: ${catdifferent}")
//
//  private val optionEqual: Boolean = optionCat1 === optionCat2
//  println(s"comparing cats options: ${catequal}")


}
