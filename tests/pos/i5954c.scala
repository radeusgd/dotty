abstract class MatcherFactory1[A] {
  class AndNotWord
}

object MatcherFactory1 {
  import scala.quoted.{_, given}

  def impl(self: Expr[MatcherFactory1[Int]#AndNotWord])(given QuoteContext) =
    '{ val a: Any = $self }


  def impl[T: Type](self: Expr[MatcherFactory1[T]#AndNotWord])(given QuoteContext) =
    '{ val a: Any = $self }

}
