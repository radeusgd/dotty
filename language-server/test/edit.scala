import scala.quoted._

private object MacroEdit {
  inline def edit(expr: => Any) <: Any =
    ${ editImpl('expr) }

  def editImpl(expr: Expr[Any])(implicit qctx: QuoteContext) = {
    import qctx.tasty._

    applyEdit(expr.pos)

    expr
  }

  def test(xs: Option[Int]) = {
    hi(xs)
  }
}
