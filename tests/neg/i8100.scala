import scala.quoted._
import scala.quoted.matching._

class M {
  type E
}

def f[T: Type](given QuoteContext) =
  summonExpr[M] match
    case Some('{ $mm : $tt }) =>
      '{
        val m = $mm
        ${ val b: m.type =
          m // error
          ???
        }
      }


def g[T](given Type[T]) = ???
