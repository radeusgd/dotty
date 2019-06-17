package scala.quoted

trait QuoteContext {

  def show[T](expr: Expr[T]): String

  def show[T](tpe: Type[T]): String

}
