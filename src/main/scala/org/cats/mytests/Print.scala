package org.cats.mytests

object Print extends App {
  trait Printable[A] {
    def format(value: A): String
    def contramap[B](func: B => A): Printable[B] =
      new Printable[B] {
        def format(value: B): String =
          Printable.this.format(func(value))
      }
  }
  object PrintableInstances {
    implicit val stringPrinter: Printable[String] = new Printable[String] {
      override def format(value: String): String = value
    }

    implicit val integerPrinter: Printable[Int] = new Printable[Int] {
      override def format(value: Int): String = value.toString
    }

    implicit val catPrinter: Printable[Cat] = new Printable[Cat] {
      override def format(cat: Cat): String =
        s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
    }

    //contravariant functor exercise
    final case class Box[A](value: A)

    implicit def boxPrinter[A](
      implicit printable: Printable[A]
    ): Printable[Box[A]] =
      printable.contramap(box => box.value)

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

  val gizmo = Cat("Gizmo", 6, "tiger")
  final case class Cat(name: String, age: Int, color: String)
  Printable.print(gizmo)

  println(Printable.format(Box("dentro")))
//  println(Printable.format(Box(true))) //does not compile because we haven't defined a Printable[Boolean] instance

  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit printer: Printable[A]): String = printer.format(value)
      def print(implicit printer: Printable[A]): Unit =
        println(printer.format(value))
    }
  }

  import org.cats.mytests.Print.PrintableSyntax.PrintableOps
  gizmo.print

}
