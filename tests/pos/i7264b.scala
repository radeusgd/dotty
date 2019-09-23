import scala.quoted._
class Foo {
  def f[T](x: Expr[T])(given QuoteContext) = x match {
  case '{ val x: *:[Int, $t] = $body } =>
  }
}
