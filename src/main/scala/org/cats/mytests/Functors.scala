package org.cats.mytests

import cats.Functor

object Functors extends App {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)
    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

  implicit def functorTree: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) =>
          Branch(this.map(left)(f), this.map(right)(f))
      }
  }

  val tree = Branch(Leaf(1), Leaf(2))
  println(functorTree.map(tree)(n => n + 1))

  import cats.implicits.toFunctorOps
  println(Tree.leaf(3).map(n => n * 3))

}
