import scala.quoted._

object Macros {

  inline def blackbox: Int = ${one}

  inline def whitebox <: Int = ${one}

  private def one(given QuoteContext): Expr[Int] = Expr(1)

}
