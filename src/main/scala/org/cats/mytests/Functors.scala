package org.cats.mytests

import cats.Functor

object Functors extends App {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

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

}
