object session {
  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)

  val x = 0

  def f(y: Int) = y + 1

  val result = {
    val x = f(3)
    x * x
  } + x

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  gcd(14, 21)

  def factorial(n: Int): Int =
    if (n == 0) 1 else n * factorial(n - 1)

  def factTailRecur(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)

    loop(1, n)
  }

  factorial(4)
  factTailRecur(4)
}
