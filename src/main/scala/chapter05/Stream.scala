package chapter05

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = if (n > 0) {
    this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n-1))
      // This doesn't seem right.  Yet, it needs to be lazy, but when it is evaluated we need to force the tail to be evaluated.
    }
  } else Empty

  def drop(n: Int): Stream[A] = if (n > 0) {
    this match {
      case Empty => Empty
      case Cons(h, t) => t().drop(n-1)
    }
  } else this

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h,t) => if (p(h)) Cons(() => h, () => t) else Empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) =>  Some(h())
  }

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h,t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h,t) => if (f(h)) cons(h, t) else t)

  def append[AA >: A](r: => Stream[AA]): Stream[AA] =
    foldRight(r)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((h,t) => f(h).append(t))

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty).forAll{case (h1, h2) => h1 == h2}

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      // their solution has a special case for the final element
      // makes sense that you would want to include empty, but why doesn't my test fail?
      // case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), x) if x > 0 => Some((h(), (t(), x-1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (_, Empty) => None
      case (Empty, _) => None
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()), (t1(),t2())))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case ((Cons(h1,t1), Cons(h2,t2))) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case ((Cons(h1,t1), Empty)) => Some((Some(h1()), None), (t1(), Empty))
      case ((Empty), Cons(h2,t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    }.append(Stream(Empty))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val c: Stream[A] = cons(a, c)
    c
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def loop(prev: Int, next: Int): Stream[Int] =
      cons(prev, loop(next, prev+next))

    loop(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some(x) => cons(x._1, unfold(x._2)(f))
    case None => Empty
  }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(z => Some(z, z+1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(z => Some(z, z))

  def onesViaUnfold: Stream[Int] =
    unfold(1)(z => Some(1,1))

  def fibsViaUnfold: Stream[Int] =
    unfold((0,1)) { case (z0,z1) => Some((z0, (z1, z0+z1))) }
}
