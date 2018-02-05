package chapter02

object Fibonacci {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, next: Int): Int = {
      if (n <= 0) prev
      else if (n == 1) next
      else go(n-1, next, next + prev)
    }

    go(n, 0, 1)
  }
}
