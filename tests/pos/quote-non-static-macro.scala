import scala.quoted.{_, given}

class Foo {
  inline def foo: Unit = ${Foo.impl}
  object Bar {
    inline def foo: Unit = ${Foo.impl}
  }
}

object Foo {
  class Baz {
    inline def foo: Unit = ${impl}
  }
  object Quox {
    inline def foo: Unit = ${Foo.impl}
  }
  def impl(given QuoteContext): Expr[Unit] = '{}
}
