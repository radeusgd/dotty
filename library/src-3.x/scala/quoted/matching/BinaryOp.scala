package scala.quoted
package matching

object BinaryOp {
  def unapply[T](expr: Expr[T]) given (refl: tasty.Reflection): BinaryOp[T] = {
    import refl._
    def isByNameMethodType(tp: Type): Boolean =  tp.widen match {
      case Type.MethodType(_, Type.ByNameType(_) :: Nil, _) => true
      case _ => false
    }
    expr.unseal match {
      case Apply(sel @ Select(arg1, op), List(arg2)) if ! isByNameMethodType(sel.tpe) =>
        new BinaryOp[T] {
          def isEmpty: Boolean = false
          def _1 = arg1.seal.asInstanceOf[Expr[X]]
          def _2 = arg2.seal.asInstanceOf[Expr[Y]]
          def _3: String = op
          def _4 =
            (left: Expr[X], right: Expr[Y]) => left.unseal.select(sel.symbol).appliedTo(right.unseal).seal.asInstanceOf[Expr[T]]
        }
      case _ =>
        new BinaryOp[T] {
          def isEmpty: Boolean = true
          def _1 = ???
          def _2 = ???
          def _3 = ???
          def _4 = ???
        }
    }
  }
}

trait BinaryOp[T] {
  type X
  type Y

  def isEmpty: Boolean

  def get: this.type = this

  def _1: Expr[X]
  def _2: Expr[Y]
  def _3: String
  def _4: (Expr[X], Expr[Y]) => Expr[T]
}
