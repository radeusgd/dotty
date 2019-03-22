import scala.quoted._

import scala.tasty._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(x: Expr[Any])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._
    val pos = x.unseal.underlyingArgument.pos
    error("here is the the argument is " + x.unseal.underlyingArgument.showCode, pos)
    error("here (+5) is the the argument is " + x.unseal.underlyingArgument.showCode, pos.withOffset(start = pos.start + 5, end = pos.end + 5))
    '{}
  }

}
