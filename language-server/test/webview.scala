import scala.quoted._

private object MacroWebview {
  inline def hi(expr: => Any) <: Any =
    ${ hiImpl('expr) }

  def hiImpl(expr: Expr[Any])(implicit qctx: QuoteContext) = {
    import qctx.tasty._

    setWebview(Webview(title = "Hello", body = s"<b>duck: ${expr.unseal}</b>"))

    expr
  }

  def test = {
    hi(1234)
  }
}

