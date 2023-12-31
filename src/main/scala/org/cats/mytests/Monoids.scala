package org.cats.mytests

object Monoids extends App {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) =
      monoid
  }

  val booleanSemigroup: Semigroup[Boolean] = (x, y) => x && y
  val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = booleanSemigroup.combine(x, y)
  }

  assert(booleanSemigroup.combine(true, booleanSemigroup.combine(true, false)) ==
  booleanSemigroup.combine(booleanSemigroup.combine(true, true), false))

  assert(booleanMonoid.combine(false, booleanMonoid.empty) == false)
  assert(booleanMonoid.combine(true, booleanMonoid.empty) == true)


}
