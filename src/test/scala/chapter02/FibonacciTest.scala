package chapter02

import org.scalatest.{FlatSpec, Matchers}

class FibonacciTest extends FlatSpec with Matchers {
  "Fibonacci" should "work for expected numbers" in {
    Fibonacci.fib(0) shouldBe 0
    Fibonacci.fib(5) shouldBe 5
    Fibonacci.fib(10) shouldBe 55
  }
}