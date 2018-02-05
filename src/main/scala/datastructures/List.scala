package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  // 3.1
  // What will be the result of the following match expression?
  val local: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // This will match and return 3
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // 3.2
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("Unable to get tail of empty list") // Returning Nil might be a good option to.
    case Cons(_, tail) => tail
  }

  // 3.3
  def setHead[A](list: List[A], element: A): List[A] = {
    Cons(element, tail(list))
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(List.tail(l), n - 1)

  // 3.5
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if f(head) => dropWhile(tail)(f)
    case _ => l
  }

  // 3.6
  def init[A](list: List[A]): List[A] = list match {
    case Nil => list
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  // 3.7 Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
  // It doesn't seem that you could since the call to the function relies on foldRight being called so foldRight will
  // continue with the evaluation

  // 3.8
  foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  // What do you think this says about foldRight and the data constructors of List?
  // What is meant by the data constructors?  Is this trying to insinuate that foldRight is following the reference?

  // 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => 1 + y)

  // 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  // 3.11
  def sumLeft(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def productLeft(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def lengthLeft(list: List[Int]): Int = foldLeft(list, 0)((x, y) => x + 1)

  // 3.12
  def reverse[A](list: List[A]): List[A] = foldLeft(list, Nil: List[A])((acc, x) => Cons(x, acc))

  // 3.13
  // Looking at their solutions I understand I'm taking too simplistic of a view.  I need to consider what the
  // functions are actually doing to get the pattern working in general cases.
  def foldLeftViaRight[A, B](list: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(list), z)((x, acc) => f(acc, x))

  def foldRightViaLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(list), z)((acc, x) => f(x, acc))

  // 3.14
  def append[A](list: List[A], r: List[A]): List[A] =
    foldRight(list, r)((x, acc) => Cons(x, acc))

  // 3.15
  def concatenate[A](list: List[List[A]]): List[A] = {
    def loop(source: List[List[A]], acc: List[A]): List[A] = source match {
      case Nil => acc
      case Cons(head, tail) => loop(tail, append(acc, head))
    }

    loop(list, Nil)
  }

  def concatenateFoldLeft[A](list: List[List[A]]): List[A] =
    foldLeft(list, Nil: List[A])((acc, x) => append(acc, x))

  // 3.16
  def addOne(list: List[Int]): List[Int] =
    foldRight(list, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  // 3.17
  def doubleToString(list: List[Double]): List[String] =
    foldRight(list, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((x, acc) => append(f(x), acc))
  // the authors implemented this with a concat of maps

  // 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(i => if (f(i)) List(i) else Nil)

  // 3.22
  def sumElements(as: List[Int], bs: List[Int]): List[Int] = (as,bs) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sumElements(t1, t2))
    case _ => Nil
  }

  // 3.23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case _ => Nil
  }

  // 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    // this fails for List(1,2,2,3,4) with List(2,3)
    def loop(orig: List[A], sublist: List[A]): Boolean = (orig, sublist) match {
      case (_, Nil) => true
      case (Nil, Cons(_, _)) => false
      case (Cons(origHead, origTail), Cons(sublistHead, sublistTail)) =>
        if (origHead == sublistHead) loop(origTail, sublistTail)
        else false
    }

    (sup, sub) match {
      case (Cons(supHead, supTail), Cons(subHead, subTail)) =>
        if (supHead == subHead) loop(supTail, subTail)
        else hasSubsequence(supTail, sub)
      case (Nil, _) => false
    }
  }
}
