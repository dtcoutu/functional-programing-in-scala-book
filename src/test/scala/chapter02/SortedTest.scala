package chapter02

import org.scalatest.{FlatSpec, Matchers}

class SortedTest extends FlatSpec with Matchers {
  "Sorted" should "be true for an empty array" in {
    Sorted.isSorted(Array[Int](), (x: Int, y: Int) => x <= y) shouldBe true
  }

  "Sorted" should "be true for a sorted array" in {
    Sorted.isSorted(Array(1,2,3), (x: Int, y: Int) => x <= y) shouldBe true
  }

  "Sorted" should "be false for a non-sorted array" in {
    Sorted.isSorted(Array(2,1,3), (x: Int, y: Int) => x <= y) shouldBe false
  }
}
