import scala.quoted._

object PowerMacro {
  def powerCode(x: Expr[Double], n: Long): Expr[Double] =
    if (n == 0) '{1.0}
  else if (n % 2 == 0) '{ val y = $x * $x; ${ powerCode('y, n / 2) } }
  else '{ $x * ${ powerCode(x, n - 1) } }

  inline def power(inline n: Long, x: Double) = ${ PowerMacro.powerCode('x, n) }
}

class Num(x: Double) {
  inline def power(inline n: Long) = ${ PowerMacro.powerCode('x, n) }
}
