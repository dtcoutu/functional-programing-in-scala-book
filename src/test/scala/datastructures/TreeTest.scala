package datastructures

import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers {
  "size" should "return the number of nodex in a tree" in {
    Tree.size(Branch(Leaf(1), Branch(Leaf(4), Leaf(5)))) shouldBe 5
  }

  "maximum" should "return the highest value in the tree" in {
    Tree.maximum(Branch(Leaf(1), Branch(Leaf(4), Leaf(5)))) shouldBe 5
    Tree.maximum(Branch(Leaf(10), Branch(Leaf(4), Leaf(5)))) shouldBe 10
  }

  "depth" should "return the maximum path length from root to any leaf" in {
    Tree.depth(Branch(Leaf(1), Branch(Leaf(4), Leaf(5)))) shouldBe 3
    Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))))) shouldBe 5
  }

  "map" should "apply the function to each leaf node in the tree" in {
    Tree.map(Branch(Leaf(1), Leaf(3)))(_ + 2) shouldBe Branch(Leaf(3), Leaf(5))
  }

  "fold" should "abstract the above functions" in {
    Tree.fold(Branch(Leaf(1), Branch(Leaf(4), Leaf(5))))(a => 1)(1 + _ + _) shouldBe 5
    Tree.fold(Branch(Leaf(1), Branch(Leaf(4), Leaf(5))))(a => a)(_ max _) shouldBe 5
    Tree.fold(Branch(Leaf(10), Branch(Leaf(4), Leaf(5))))(a => a)(_ max _) shouldBe 10
    Tree.fold(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))))(a => 1)((b,c) => 1 + (b max c)) shouldBe 5
//    Tree.fold(Branch(Leaf(1), Leaf(3)))(a => Leaf(a + 2)) ((b,c) => Branch(b, c)) shouldBe Branch(Leaf(3), Leaf(5))
  }
}
