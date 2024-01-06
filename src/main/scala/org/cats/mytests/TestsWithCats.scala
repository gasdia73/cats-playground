package org.cats.mytests

import java.util.Date

object TestsWithCats extends App {

  import cats.Show
  import cats.instances.int._

  val showInt = Show.apply[Int]
  println(showInt.show(345))

  import cats.syntax.show._
  println(432.show)

  implicit val dateShow: Show[Date] =
    (date: Date) => s"${date.getTime}ms since the epoch."

  println(new Date().show)

}
