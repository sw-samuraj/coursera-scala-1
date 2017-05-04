val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.numer
x.denom
x + y
x - y - z
y + y
x < y
x.max(y)
// val strange = new Rational(1, 0)
// strange.add(strange)
new Rational(2)

x max y
-x
-y
-z

class Rational(x: Int, y: Int) {
  require (y != 0, "denominator must be non-zero")

  def numer = x
  def denom = y

  def this(x: Int) = this(x, 1)

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  def < (that: Rational) =
    numer * that.denom < that.numer * denom

  def max(that: Rational) =
    if (this < (that)) that else this

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }
}