import scala.annotation.tailrec

class Rational(x: Int, y: Int):
  require(y > 0, s"Denominator must be positive, was $x/$y")

  val numer = x / gcd(x.abs, y)
  val denom = y / gcd(x.abs, y)

  def this(x: Int) = this(x, 1)

  def add(r: Rational) =
    new Rational(numer * r.denom + r.numer * denom,
      denom * r.denom)

  def mul(r: Rational) =
    new Rational(numer * r.numer, denom * r.denom)

  def sub(r: Rational) = add(r.neg())

  def neg() =
    new Rational(-numer, denom)

  def less(that: Rational): Boolean =
    numer * that.denom < that.numer * denom

  def max(that: Rational): Rational =
    if this.less(that) then that else this

  @tailrec
  private def gcd(a: Int, b:Int): Int = {
    if b == 0 then a else gcd(b, a % b)
  }

  override def toString = s"$numer/$denom"
end Rational

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
val h = new Rational(2, 6)
val k = new Rational(2)

println(x.add(y).mul(z))
println(x.sub(y).add(z.neg()))
