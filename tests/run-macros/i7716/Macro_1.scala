import scala.quoted.{ given, _ }

trait Foo
  def mcrImpl1(e: Expr[Any])(given ctx: QuoteContext): Expr[Any] =
    '{println(s"Hello ${$e}")}

object Foo extends Foo
  def mcrImpl2(e: Expr[Any])(given ctx: QuoteContext): Expr[Any] =
    '{println(s"Hello ${$e}")}

object Bar
  import Foo._
  inline def mcr1(e: => Any) = ${mcrImpl1('e)}

  inline def mcr2(e: => Any) = ${Foo.mcrImpl1('e)}

  inline def mcr3(e: => Any) = ${mcrImpl2('e)}
