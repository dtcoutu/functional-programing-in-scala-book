package chapter04

import org.scalatest.{FlatSpec, Matchers}
import chapter04.Either

class EitherTest extends FlatSpec with Matchers {
  "map" should "work" in {
    Right(5).map(a => a * 2) shouldBe Right(10)
  }

  "traverse" should "work" in {
    Either.traverse(List(Right(4), Right(3), Left("bad"))) {
      case Left(e) => Left(e)
      case Right(aa) => Right(aa * 2)
    } shouldBe Left("bad")

    Either.traverse(List(Right(4), Left("bad"), Left("wrong"))) {
      case Left(e) => Left(e)
      case Right(aa) => Right(aa * 2)
    } shouldBe Left("bad")
  }

  "sequence" should "work" in {
    Either.sequence(List(Right(4), Left("bad"), Left("wrong"))) shouldBe Left("bad")
    Either.sequence(List(Right(4), Right(3), Right(2))) shouldBe Right(List(4,3,2))
  }
}
