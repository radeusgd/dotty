import scala.quoted._

private object MacroCompletion {
  inline def hi(expr: => Any) <: Any =
    ${ hiImpl('expr) }

  def hiImpl(expr: Expr[Any])(implicit qctx: QuoteContext) = {
    import qctx.tasty._

    setCompletions(List(Completion("llabel", "ddescription", Nil)))

    expr
  }

  def test = {
    hi(1234)
  }
}
