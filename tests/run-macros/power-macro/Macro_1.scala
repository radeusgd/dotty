
import scala.quoted._

// inline def power(x: Double, inline n: Int) = ${ powerCode1('x, 'n) }

// private def powerCode1(using qctx: QuoteCtx)(x: qctx.Expr[Double], n: qctx.Expr[Int]): qctx.Expr[Double] =
//   powerCode(x, n.value)

private def powerCode(using qctx: QuoteCtx)(x: qctx.Expr[Double], n: Int): qctx.Expr[Double] =
  if (n == 0) Expr(1.0)
  else if (n == 1) x
  else if (n % 2 == 0) '{ val y = $x * $x; ${ powerCode('y, n / 2) } }
  else '{ $x * ${ powerCode(x, n - 1) } }
