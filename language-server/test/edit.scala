import scala.quoted._

private object MacroEdit {
  inline def hi(expr: => Any) <: Any =
    ${ hiImpl('expr) }

  def hiImpl(expr: Expr[Any])(implicit qctx: QuoteContext) = {
    import qctx.tasty._

    applyEdit(List(TextEdit(expr.unseal.pos, "hello")))

    expr
  }

  def test = {
    hi(1234)
  }
}
