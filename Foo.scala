trait QuoteCtx { self =>
  type Expr[+T]

  type NestedContext = QuoteCtx {
    type Expr[+T] >: self.Expr[T]

  }
}

def exprSpliced[T, QCtx <: QuoteCtx](x: (qctxx: QCtx) ?=> qctxx.Expr[T]): T = ???

def test =
  val q1: QuoteCtx = ???
  val x: q1.Expr[Int] = ???
  exprSpliced[QCtx = q1.NestedContext]((q: q1.NestedContext) ?=> x)
