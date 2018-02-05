package chapter04

import org.scalatest.{FlatSpec, Matchers}

class OptionTest extends FlatSpec with Matchers {
  "map" should "work" in {
    val f = (a: Int) => a + 2

    Some(3).map(f) shouldBe Some(5)

    None.map(f) shouldBe None
  }

//  "flatMap" should "work" in {
//
//  }

  "getOrElse" should "work" in {
    Some(3).getOrElse(10) shouldBe 3
    None.getOrElse(10) shouldBe 10
  }

  "orElse" should "work" in {
    Some(3).orElse(Some(10)) shouldBe Some(3)
    None.orElse(Some(10)) shouldBe Some(10)
  }

  "filter" should "work" in {
    Some(3).filter(_ < 5) shouldBe Some(3)
    Some(3).filter(_ > 5) shouldBe None
  }

  "variance" should "work" in {
    Option.variance(Seq(1,2,10,12,15)) shouldBe Some(30.8)
    Option.variance(Seq()) shouldBe None
  }

  "map2" should "work" in {
    Option.map2(None, None)((a,b) => 5) shouldBe None
    Option.map2(None, Some(1))((a,b) => 5) shouldBe None
    Option.map2(Some(1), None)((a,b) => 5) shouldBe None
    Option.map2(Some(1), Some(6))(_ + _) shouldBe Some(7)
  }

  "sequence" should "work" in {
    Option.sequence(List(Some(1), Some(3), Some(5))) shouldBe Some(List(1,3,5))
  }

  "sequenceViaTraverse" should "work" in {
    Option.sequenceViaTraverse(List(Some(1), Some(3), Some(5))) shouldBe Some(List(1,3,5))
  }

//  "traverse" should "work" in {
//    Option.traverse(List(Some(1), Some(3), Some(5)))() shouldBe Some(List(1,3,5))
//  }
}
