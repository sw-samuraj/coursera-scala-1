object higherOrderFunctions {

  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)

  /*
  def sumInts(a: Int, b: Int) = sum(id, a, b)
  def sumCubes(a: Int, b: Int) = sum(cube, a, b)
  */
  def sumFactorials(a: Int, b: Int) = sum(fact, a, b)

  def id(x: Int): Int = x
  def cube(x: Int): Int = x * x * x
  def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

  def sumInts(a: Int, b: Int) = sum(x => x, a, b)
  def sumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)

  sumInts(12, 42)
  sumCubes(12, 42)
  sumFactorials(2, 12)

  def sumTailRecur(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }

  def sumTailInts(a: Int, b: Int) = sumTailRecur(x => x, a, b)
  def sumTailCubes(a: Int, b: Int) =
    sumTailRecur(x => x * x * x, a, b)

  sumTailInts(12, 42)
  sumTailCubes(12, 42)
}
