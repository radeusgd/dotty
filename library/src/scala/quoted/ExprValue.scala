package scala.quoted

import scala.quoted.matching.Literal

/** A typeclass for `quoted.Expr[T]` that can be turn into `T` */
trait ExprValue[T] {
  def valueOf(expr: Expr[T])(implicit reflect: tasty.Reflection): Option[T]
}

object ExprValue {

  implicit def ExprValueBooleanExpr: ExprValue[Boolean] = new ExprValue[Boolean] {
    def valueOf(expr: Expr[Boolean])(implicit reflect: tasty.Reflection): Option[Boolean] = Literal.unapply(expr)
  }

  implicit def ExprValueByteExpr: ExprValue[Byte] = new ExprValue[Byte] {
    def valueOf(expr: Expr[Byte])(implicit reflect: tasty.Reflection): Option[Byte] = Literal.unapply(expr)
  }

  implicit def ExprValueShortExpr: ExprValue[Short] = new ExprValue[Short] {
    def valueOf(expr: Expr[Short])(implicit reflect: tasty.Reflection): Option[Short] = Literal.unapply(expr)
  }

  implicit def ExprValueIntExpr: ExprValue[Int] = new ExprValue[Int] {
    def valueOf(expr: Expr[Int])(implicit reflect: tasty.Reflection): Option[Int] = Literal.unapply(expr)
  }

  implicit def ExprValueLongExpr: ExprValue[Long] = new ExprValue[Long] {
    def valueOf(expr: Expr[Long])(implicit reflect: tasty.Reflection): Option[Long] = Literal.unapply(expr)
  }

  implicit def ExprValueFloatExpr: ExprValue[Float] = new ExprValue[Float] {
    def valueOf(expr: Expr[Float])(implicit reflect: tasty.Reflection): Option[Float] = Literal.unapply(expr)
  }

  implicit def ExprValueDoubleExpr: ExprValue[Double] = new ExprValue[Double] {
    def valueOf(expr: Expr[Double])(implicit reflect: tasty.Reflection): Option[Double] = Literal.unapply(expr)
  }

  implicit def ExprValueCharExpr: ExprValue[Char] = new ExprValue[Char] {
    def valueOf(expr: Expr[Char])(implicit reflect: tasty.Reflection): Option[Char] = Literal.unapply(expr)
  }

  implicit def ExprValueStringExpr: ExprValue[String] = new ExprValue[String] {
    def valueOf(expr: Expr[String])(implicit reflect: tasty.Reflection): Option[String] = Literal.unapply(expr)
  }

}
