package scala.quoted

final class SealedExpr private (e: Expr[_], t: Type[_]) {

  type T

  implicit def tpe: scala.quoted.Type[T] = t.asInstanceOf[scala.quoted.Type[T]]

  def expr: scala.quoted.Expr[T] = e.asInstanceOf[scala.quoted.Expr[T]]

}

object SealedExpr {
  def apply[T](expr: Expr[T])(implicit tpe: Type[T]): SealedExpr = new SealedExpr(expr, tpe)
}