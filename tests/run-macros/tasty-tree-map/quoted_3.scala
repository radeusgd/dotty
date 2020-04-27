import scala.quoted._

object MacrosImpl {

  def impl[T: Type](x: Expr[T])(using qctx: QuoteContext) : Expr[T] = {
    import qctx.tasty.{_, given _}
    val identityMap = new TreeMap { }
    val transformed = identityMap.transformTerm(x.unseal).seal.cast[T]
    transformed
  }

}
