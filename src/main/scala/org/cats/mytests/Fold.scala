package org.cats.mytests

object Fold extends App {

  val res = List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a)
  val res2 = List(1, 2, 3).foldRight(List.empty[Int])((a, i) => a :: i)

  println(res)
  println(res2)

  def foldMap[A, B](fa: List[A])(f: A => B): List[B] =
    fa.foldLeft(List.empty[B])((listB, a) => f(a) :: listB)

  println(foldMap(List(4, 4, 4, 4))(_ * 2))

  def foldFlatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
    fa.foldLeft(List.empty[B])((accum, item) => f(item) ::: accum)

  println(foldFlatMap(List(4, 4, 4, 4))(a => List(a, 2)))

}
