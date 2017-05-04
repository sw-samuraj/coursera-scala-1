object currying {

  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    sumF
  }

  def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

  def sumInts = sum(x => x)
  def sumCubes = sum(x => x * x * x)
  def sumFactorials = sum(fact)

  sumCubes(1, 10) + sumFactorials(10, 20)

  sum(x => x * x * x)(1, 10)
}