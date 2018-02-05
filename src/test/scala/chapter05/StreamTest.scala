package chapter05

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {
  "toList" should "work" in {
    Stream(1,2,3).toList shouldBe List(1,2,3)
  }

  "take" should "work" in {
    Stream('a', 'b', 'c').take(2).toList shouldBe List('a', 'b')
  }

  "drop" should "work" in {
    Stream(1,2,3,4).drop(2).toList shouldBe List(3,4)
    Stream(1).drop(2) shouldBe Empty
  }

  "takeWhile" should "work" in {
    Stream(1,2,3,4).takeWhile(_ < 2).toList shouldBe List(1)
    Stream(10,2,3,4).takeWhile(_ < 2) shouldBe Empty
  }

  "forAll" should "work" in {
    Stream(1,2,3,4).forAll(_ < 5) shouldBe true
    Stream(1,2,3,4).forAll(_ < 4) shouldBe false
  }

  "takeWhileViaFoldRight" should "work" in {
    Stream(1,2,3,4).takeWhileViaFoldRight(_ < 2).toList shouldBe List(1)
    Stream(1,2,3,4).takeWhileViaFoldRight(_ < 3).toList shouldBe List(1,2)
    Stream(1,2,3,1,4).takeWhileViaFoldRight(_ < 3).toList shouldBe List(1,2)
    Stream(10,2,3,4).takeWhileViaFoldRight(_ < 2) shouldBe Empty
  }

  "map" should "work" in {
    Stream(1,2,3,4).map(_ + 2).toList shouldBe List(3,4,5,6)
  }

  "filter" should "work" in {
    Stream(1,2,3,4).filter(_ % 2 == 0).toList shouldBe List(2,4)
  }

  "append" should "work" in {
    Stream('a', 'b').append(Stream('x', 'y')).toList shouldBe List('a','b','x','y')
    Stream('a', 'b').append(Empty).toList shouldBe List('a','b')
    Empty.append(Stream('x','y')).toList shouldBe List('x','y')
  }

  "flatMap" should "work" in {
    Stream("1", "2", "foo", "3", "bar").flatMap(a => if (a forall Character.isDigit) Stream(a.toInt) else Empty).toList shouldBe List(1,2,3)
  }

  "fibs" should "work" in {
    Stream.fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  "unfold" should "work" in {
    Stream.unfold(0)(a => Some(a, a+1)).take(5).toList shouldBe List(0,1,2,3,4)
  }

  "fibsViaUnfold" should "work" in {
    Stream.fibsViaUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  "mapViaUnfold" should "work" in {
    Stream(1,2,3,4).mapViaUnfold(_ + 2).toList shouldBe List(3,4,5,6)
  }

  "takeViaUnfold" should "work" in {
    Stream('a', 'b', 'c').takeViaUnfold(2).toList shouldBe List('a', 'b')
  }

  "takeWhileViaUnfold" should "work" in {
    Stream(1,2,3,4).takeWhileViaUnfold(_ < 2).toList shouldBe List(1)
    Stream(1,2,3,4).takeWhileViaUnfold(_ < 3).toList shouldBe List(1,2)
    Stream(1,2,3,1,4).takeWhileViaUnfold(_ < 3).toList shouldBe List(1,2)
    Stream(10,2,3,4).takeWhileViaUnfold(_ < 2) shouldBe Empty
  }

  "zipWith" should "work" in {
    Stream(1,2,3,4).zipWith(Stream(5,6,7))(_ + _).toList shouldBe List(6, 8, 10)
  }

  "zipAll" should "work" in {
    Stream(1,2,3,4).zipAll(Stream("a","b","c")).toList shouldBe List((Some(1),Some("a")), (Some(2),Some("b")), (Some(3),Some("c")), (Some(4), None))
  }

  "startsWith" should "work" in {
    Stream(1,2,3,4).startsWith(Stream(1,2)) shouldBe true
    Stream(1,2,3,4).startsWith(Stream(2,3)) shouldBe false
  }

  "tails" should "work" in {
    Stream(1,2,3).tails.map(_.toList).toList shouldBe List(List(1,2,3),List(2,3),List(3), List())
  }
}
