package org.cats.mytests

object Monoids extends App {

  trait MySemigroup[A] {
    def combine(x: A, y: A): A
  }

  trait MyMonoid[A] extends MySemigroup[A] {
    def empty: A
  }

  object MyMonoid {
    def apply[A](implicit monoid: MyMonoid[A]) =
      monoid
  }

  //boolean monoids

  val booleanSemigroup: MySemigroup[Boolean] = (x, y) => x && y
  val booleanMonoid: MyMonoid[Boolean] = new MyMonoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = booleanSemigroup.combine(x, y)
  }

  assert(booleanSemigroup.combine(true, booleanSemigroup.combine(true, false)) ==
  booleanSemigroup.combine(booleanSemigroup.combine(true, true), false))

  assert(booleanMonoid.combine(false, booleanMonoid.empty) == false)
  assert(booleanMonoid.combine(true, booleanMonoid.empty) == true)

  //sets monoids
  val setSemigroup : MySemigroup[Set[_]] = (s1, s2) => s1.concat(s2)
  val setMonoid : MyMonoid[Set[_]] = new MyMonoid[Set[_]] {
    override def empty: Set[_] = Set.empty
    override def combine(x: Set[_], y: Set[_]): Set[_] = setSemigroup.combine(x, y)
  }

  import cats.instances.int._
  import cats.syntax.semigroup._

//
//  def add(items: List[Int]): Int =
//    items match {
//      case x :: xs => x |+| add(xs)
//      case _ => 0
//    }

  //or

  import cats.Monoid

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldLeft(monoid.empty)(_ |+| _)

  println(add(List(1,3,5,7)))
}
