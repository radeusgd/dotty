import scala.quoted._

private object MacroResult {
  inline def hi(expr: => Any) <: Any =
    ${ hiImpl('expr) }

  def hiImpl(expr: Expr[Any])(implicit qctx: QuoteContext) = {
    import qctx.tasty._

    setResults(List(Result(expr.unseal.pos, "bar")))

    expr
  }

  def test = {
    hi(1234)
  }
}
