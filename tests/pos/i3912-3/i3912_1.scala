import scala.quoted.{_, given}

object Macros {
  inline def foo3(): Int = {
    {
      ${ impl() }
    }
  }

  def impl()(given QuoteContext): Expr[Int] = '{1}
}