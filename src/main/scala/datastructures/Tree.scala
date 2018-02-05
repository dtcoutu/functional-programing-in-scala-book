package datastructures

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Branch(t1, t2) => 1 + size(t1) + size(t2)
    case Leaf(_) => 1
  }

  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(t1, t2) => maximum(t1) max maximum(t2)
    case Leaf(i) => i
  }

  // 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Branch(t1, t2) => 1 + (depth(t1) max depth(t2))
    case Leaf(_) => 1
  }

  // 3.28
  def map[A,B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Branch(t1, t2) => Branch(map(t1)(f), map(t2)(f))
    case Leaf(v) => Leaf(f(v))
  }

  // 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(v) => f(v)
  }

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
