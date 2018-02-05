package datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {
  "List tail" should "return tail with a populated list" in {
    List.tail(List(1,2,3)) shouldBe List(2,3)
    List.tail(List(1,2)) shouldBe List(2)
    List.tail(List(1)) shouldBe List()
  }

  it should "return Nil with an empty list" in {
    assertThrows[RuntimeException] {
      List.tail(List())
    }
  }

  "List setHead" should "add the given element to the tail of the given list" in {
    List.setHead(List("humpty", "dumpty"), "grover") shouldBe List("grover", "dumpty")

    assertThrows[RuntimeException] {
      List.setHead(List(), "bummer")
    }
  }

  "List drop" should "drop the correct number of elements" in {
    List.drop(List(1,2,3,4,5,6), 2) shouldBe List(3,4,5,6)
  }

  "List dropWhile" should "only drop elements that match the function" in {
    // I misunderstood the function definition.
    //List.dropWhile(List(1,2,3,4,5,6,7,8,9,10), (x: Int) => x%2 == 0) shouldBe List(1,3,5,7,9)

    List.dropWhile(List(1,2,3,4,5,6,7,8,9,10))(_ < 5) shouldBe List(5,6,7,8,9,10)
  }

  "List init" should "return the list without the last element" in {
    List.init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
  }

  "List foldRight" should "work with Cons" in {
    List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) shouldBe Cons(1, Cons(2, Cons(3, Nil)))
  }

  "List length" should "return the length of the list" in {
    List.length(List(1,2,3)) shouldBe 3
  }

  "List foldLeft" should "be able to sum values" in {
    List.foldLeft(List(5,10,3), 0)(_ + _) shouldBe 18

    List.foldLeft(List(1, 5, 10), Nil: List[Int])((acc, x) => Cons(x+1, acc)) shouldBe List(11, 6, 2)
  }

  "List sumLeft" should "sum values in a list" in {
    List.sumLeft(List(5,8,8)) shouldBe 21
  }

  "List productLeft" should "multiple values in a list" in {
    List.productLeft(List(5, 8, 10)) shouldBe 400
  }

  "List lengthLeft" should "return the length of the list" in {
    List.lengthLeft(List(1,2,3)) shouldBe 3
  }

  "List reverse" should "return the list in opposite order" in {
    List.reverse(List(1,3,2)) shouldBe List(2,3,1)
  }

  "List foldLeftViaRight" should "be able to sum values" in {
    List.foldLeftViaRight(List(5,10,3), 0)(_ + _) shouldBe 18

    List.foldLeftViaRight(List(1, 5, 10), Nil: List[Int])((acc, x) => Cons(x+1, acc)) shouldBe List(11, 6, 2)
  }

  "List foldRightViaLeft" should "be able to sum values" in {
    List.foldRightViaLeft(List(4,8,5), 0)(_ + _) shouldBe 17

    List.foldRightViaLeft(List(1, 5, 10), Nil: List[Int])((x, acc) => Cons(x+1, acc)) shouldBe List(2, 6, 11)
  }

  "append" should "add the elements to the end of the list" in {
    List.append(List("a","b"), List("x","y","z")) shouldBe List("a","b","x","y","z")
  }

  "concatenate" should "flatten a list of lists into just a list" in {
    List.concatenate(List(List(1,2), List(5,6), List(10,11))) shouldBe List(1,2,5,6,10,11)
  }

  "concatenateFoldLeft" should "flatten a list of lists into just a list" in {
    List.concatenateFoldLeft(List(List(1,2), List(5,6), List(10,11))) shouldBe List(1,2,5,6,10,11)
  }

  "addOne" should "add one to each value" in {
    List.addOne(List(1,2,3,4)) shouldBe List(2,3,4,5)
  }

  "doubleToString" should "convert doubles to strings" in {
    List.doubleToString(List(5.0, 3.14, 45.45)) shouldBe List("5.0", "3.14", "45.45")
  }

  "map" should "modify values in a list" in {
    List.map(List(1,2,3,4))(_ + 1) shouldBe List(2,3,4,5)
    List.map(List(5.0, 3.14, 45.45))(_.toString) shouldBe List("5.0", "3.14", "45.45")
  }

  "filter" should "only return elements that reolve to true" in {
    List.filter(List(1,2,3,4,5,6,7,8,9,10))(_ % 2 == 0) shouldBe List(2,4,6,8,10)
  }

  "flatMap" should "return a single list" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) shouldBe List(1,1,2,2,3,3)
  }

  "filterViaFlatMap" should "only return elements that reolve to true" in {
    List.filterViaFlatMap(List(1,2,3,4,5,6,7,8,9,10))(_ % 2 == 0) shouldBe List(2,4,6,8,10)
  }

  "sumElements" should "return a list of elements summed from the two given lists" in {
    List.sumElements(List(1,2,3), List(4,5,6)) shouldBe List(5,7,9)
  }

  "zipWith" should "combine two lists with the given function" in {
    List.zipWith(List("a", "b", "c"), List("x", "y", "z"))(_ + _) shouldBe List("ax", "by", "cz")
  }

  "subsequence" should "correctly indicated if the list contains the subsequence" in {
    val list = List(1,5,8,10,3)
    List.hasSubsequence(list, List(1,5)) shouldBe true
    List.hasSubsequence(list, List(1,8)) shouldBe false
    List.hasSubsequence(list, List(5,8,10)) shouldBe true
    List.hasSubsequence(list, List(10,3)) shouldBe true
    List.hasSubsequence(list, List(10,3,5)) shouldBe false
  }
}
