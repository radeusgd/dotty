import scala.quoted.{_, given}

object Foo {
  inline def foo2(): Unit = ${foo2Impl()}
  def foo2Impl()(given QuoteContext): Expr[Unit] = '{}
  inline def foo(): Unit = foo2()
}
