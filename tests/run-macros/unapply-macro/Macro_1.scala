import scala.quoted._

object ReversedFoo {

  inline def unapply(x: String): Option[String] = ${ impl('x) }

  private def impl(using QuoteContext)(x: Expr[String]): Expr[Option[String]] =
    '{ if $x == "foo" then Some("off") else None }

}
