package org.cats.mytests

import cats.Show

object Print extends App {
  trait Printable[A] {
    def format(value: A): String
  }
  object PrintableInstances {
    implicit val stringPrinter: Printable[String] = new Printable[String] {
      override def format(value: String): String = value
    }

    implicit val integerPrinter: Printable[Int] = new Printable[Int] {
      override def format(value: Int): String = value.toString
    }
  }
  object Printable {
    def format[A](value: A)(implicit printer: Printable[A]): String =
      printer.format(value)
    def print[A](value: A)(implicit printer: Printable[A]): Unit =
      println(printer.format(value))
  }

  import org.cats.mytests.Print.PrintableInstances._
  Printable.print("miao!")
  Printable.print(7)


  println(Printable.format(4))


}
