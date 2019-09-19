import scala.quoted.{_, given}

object TypeToolbox {
  inline def show[A]: String = ${ showImpl('[A]) }
  private def showImpl[A, B](a: Type[A])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    a.show.toExpr
  }
}
