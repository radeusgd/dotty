import scala.quoted._
import scala.quoted.matching._

class M {
  type E
}

def f(given QuoteContext): Expr[Any] =
  val mm: Expr[M] = ???
  '{
    val m: M = $mm
    type ME = m.E
    ${ g[ME](given '[ME]) }
    ${ g[m.E](given '[ME]) }
    ${ g[ME](given '[m.E]) }
    ${ g[m.E](given '[m.E]) }
    ${ g[ME] }
    ${ g[m.E] }
    ${ g(given '[ME]) }
    ${ g(given '[m.E]) }
  }


def g[T](given Type[T]) = ???
