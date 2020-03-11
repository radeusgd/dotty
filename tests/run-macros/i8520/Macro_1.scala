import scala.quoted._

inline def test[T]: String = ${ impl('[T]) }

private def impl[T](using qctx: QuoteContext)(tp: Type[T]): Expr[String] = {
  import qctx.tasty._
  val tycontree = tp.unseal.tpe.asInstanceOf[AppliedType].tycon.asInstanceOf[TypeRef].typeSymbol.tree.asInstanceOf[TypeDef].rhs.asInstanceOf[TypeTree]

  println()
  println(tycontree)
  println()
  println()
  '{""}
}
