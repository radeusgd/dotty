package scala.quoted
package matching

/** Matches expressions containing literal constant values and extracts the value.
 *  It may match expressions of type Boolean, Byte, Short, Int, Long,
 *  Float, Double, Char, String, ClassTag, scala.Symbol, Null and Unit.
 *
 *  Usage:
 *  ```
 *  (x: Expr[B]) match {
 *    case Const(value: B) => ...
 *  }
 *  ```
 */
object Const {

  def unapply[T](expr: Expr[T])(using qctx: QuoteContext): Option[T] = {
    import qctx.tasty.{_, given _}
    def rec(tree: Term): Option[T] = tree match {
      case Literal(c) => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => rec(e)
      case Typed(e, _) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(expr.unseal)
  }

  object Expr {

    def unapply[T](tpt: Type[T])(using qctx: QuoteContext): Option[Expr[T]] = {
      import qctx.tasty.{_, given _}
      def rec(tpe: Type): Option[Expr[T]] = tpe match {
        case ConstantType(c) => Some(Literal(c).seal.asInstanceOf[Expr[T]])
        case _  => None
      }
      rec(tpt.unseal.tpe.dealias)
    }

  }
}
