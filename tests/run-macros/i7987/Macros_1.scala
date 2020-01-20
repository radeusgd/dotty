import scala.quoted._
import scala.deriving._
import scala.quoted.matching._

object Macros {
  inline def m(): Unit = ${ macroImpl() }

  def macroImpl[T]()(given qctx: QuoteContext): Expr[Unit] = {
    summonExpr[Mirror.Of[Some[Int]]]
    '{()}
  }
}