import scala.quoted._

private object Macro {
  // enum Color {
  //   case Red, Green, Blue
  // }

  inline def hi(expr: => Any) <: Any =
    ${ hiImpl('expr) }

  def hiImpl(expr: Expr[Any])(implicit qctx: QuoteContext) = {
    import qctx.tasty._
    // Literal(Constant(rootContext.owner.toString)).seal
    //val tpe = typeOf[Boolean].seal.asInstanceOf[quoted.Type[Any]]
    val cls = expr.unseal.underlyingArgument.tpe.widen.classSymbol.get

    val tpe1 = cls.namedType.seal.asInstanceOf[quoted.Type[Any]]
    //Typed('{???}.unseal, tpe.seal.unseal).seal
    // '{ ??? : $tpe}
    // '{
    //   $expr match {
    //     case e1: $tpe =>
    //       ???
    //   }
    // }

    val tpes = cls.children.map(_.namedType.fullyApplied)
    // Literal(Constant(tpes.toString)).seal
    // Match(expr.unseal.underlyingArgument, List(
    //   CaseDef(Pattern.TypeTest(tpe.unseal),None,'{???}.unseal)
    // )).seal

    Match(expr.unseal.underlyingArgument, tpes.map { tpe =>
      CaseDef(Pattern.TypeTest(tpe.seal.unseal), None, '{???}.unseal)
    }).seal
  }

  def test(xs: Option[Int]) = {
    hi(xs)
  }
}
