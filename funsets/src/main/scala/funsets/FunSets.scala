package funsets


/**
  * 2. Purely Functional Sets.
  */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = (x: Int) => x == elem

  /**
    * Returns the set of the two given elements.
    */
  def doubletonSet(first: Int, second: Int): Set = (x: Int) => x == first || x == second

  /**
    * Returns the set of the range between min and max elements.
    */
  def rangeSet(min: Int, max: Int): Set = (x: Int) => x >= min && x <= max

  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

  /**
    * Returns true if the given number is even.
    */
  def even(x: Int): Boolean = 0 == x % 2

  /**
    * Returns true if the given number is odd.
    */
  def odd(x: Int): Boolean = !even(x)

  /**
    * Returns true if the given number is positive.
    */
  def pos(x: Int): Boolean = x > 0

  /**
    * Returns true if the given number is positive.
    */
  def neg(x: Int): Boolean = x < 0

  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (s(a) && !p(a)) false
      else if (a > bound) true
      else iter(a + 1)
    }

    iter(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def loop(x: Int): Boolean = {
      if (s(x) && p(x)) true
      else if (x > bound) false
      else loop(x + 1)
    }

    loop(-bound)
  }

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = ???

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }
}
