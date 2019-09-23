import scala.quoted._
class Foo {
  def f[T](x: Type[T])(given QuoteContext) = x match {
    case '[ *:[Int, $t] ] =>
  }
}
