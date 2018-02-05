package chapter06

import chapter06.RNG.Simple
import org.scalatest.{FlatSpec, Matchers}

class StateTest extends FlatSpec with Matchers {
  "nonNegativeInt" should "work" in {
    RNG.nonNegativeInt(Simple(42))._1 shouldBe 16159453
  }

  "double" should "work" in {
    RNG.double(Simple(42))._1 shouldBe 0.007524831686168909
  }

  "intDouble" should "work" in {
    RNG.intDouble(Simple(42))._1 shouldBe (16159453, 0.5967354853637516)
  }

  "doubleInt" should "work" in {
    RNG.doubleInt(Simple(42))._1 shouldBe (0.007524831686168909, -1281479697)
  }

  "double3" should "work" in {
    RNG.double3(Simple(42))._1 shouldBe (0.007524831686168909, 0.5967354853637516, 0.15846728440374136)
  }

  "ints" should "work" in {
    RNG.ints(2)(Simple(42))._1 shouldBe List(-1281479697, 16159453)
  }

  "doubleViaMap" should "work" in {
    RNG.doubleViaMap(Simple(42))._1 shouldBe 0.007524831686168909
  }

  "randIntDouble" should "work" in {
    RNG.randDoubleInt(Simple(42))._1 shouldBe (0.007524831686168909, -1281479697)
  }

  "intsSeq" should "work" in {
    RNG.intsSeq(2)(Simple(42))._1 shouldBe List(16159453, -1281479697)
  }

  "nonNegativeLessThan" should "work" in {
    RNG.nonNegativeLessThan(100)(Simple(42))._1 should be < 100
  }

  "simulateMachine" should "accept Coin and become unlocked" in {
    val actualState = State.simulateMachine(List(Coin)).run(Machine(locked = true, 10, 0))
    actualState._1 shouldBe (10, 1)
    actualState._2 shouldBe Machine(locked = false, 10, 1)
  }

  it should "ignore Coin and remain unlocked" in {
    val actualState = State.simulateMachine(List(Coin)).run(Machine(locked = false, 10, 0))
    actualState._1 shouldBe (10, 0)
    actualState._2 shouldBe Machine(locked = false, 10, 0)
  }

  it should "dispense candy when unlocked and turned" in {
    val actualState = State.simulateMachine(List(Turn)).run(Machine(locked = false, 10, 1))
    actualState._1 shouldBe (9, 1)
    actualState._2 shouldBe Machine(locked=true, 9, 1)
  }

  it should "ignore Turn on locked machine" in {
    val actualState = State.simulateMachine(List(Turn)).run(Machine(locked = true, 10, 1))
    actualState._1 shouldBe (10, 1)
    actualState._2 shouldBe Machine(locked=true, 10, 1)
  }

  it should "process the purchase of 4 candies correctly" in {
    val endState = State.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
      .run(Machine(locked = true, 10, 10))

    endState._1 shouldBe (6, 14)
    endState._2 shouldBe Machine(locked=true, 6, 14)
  }

  it should "not accept a coin with no candy to dispense" in {
    val actualState = State.simulateMachine(List(Coin)).run(Machine(locked = true, 0, 1))
    actualState._1 shouldBe (0, 1)
    actualState._2 shouldBe Machine(locked=true, 0, 1)
  }

  it should "not dispense candy if none are available" in {
    val actualState = State.simulateMachine(List(Turn)).run(Machine(locked = false, 0, 1))
    actualState._1 shouldBe (0, 1)
    actualState._2 shouldBe Machine(locked=false, 0, 1)
  }
}
