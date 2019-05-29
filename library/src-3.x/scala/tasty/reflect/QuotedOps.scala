package scala.tasty.reflect

/** Extension methods on scala.quoted.{Expr|Type} to convert to scala.tasty.Tasty objects */
trait QuotedOps extends Core { self: Printers =>

  implicit class QuotedExprAPI[T](expr: scala.quoted.Expr[T]) {
    /** View this expression `quoted.Expr[T]` as a `Term` */
    def unseal(implicit ctx: Context): Term =
      kernel.QuotedExpr_unseal(expr)

    /** Checked cast to a `quoted.Expr[U]` */
    def cast[U: scala.quoted.Type](implicit ctx: Context): scala.quoted.Expr[U] =
      kernel.QuotedExpr_cast[U](expr)

    /** Show a source code like representation of this expression */
    def show(implicit ctx: Context): String =
      unseal.show
  }

  implicit class QuotedTypeAPI[T <: AnyKind](tpe: scala.quoted.Type[T]) {
    /** View this expression `quoted.Type[T]` as a `TypeTree` */
    def unseal(implicit ctx: Context): TypeTree =
      kernel.QuotedType_unseal(tpe)

    /** Show a source code like representation of this type */
    def show(implicit ctx: Context): String =
      unseal.show
  }

  implicit class TermToQuotedAPI(term: Term) {
    /** Convert `Term` to an `quoted.Expr[Any]` */
    def seal(implicit ctx: Context): scala.quoted.Expr[Any] =
      kernel.QuotedExpr_seal(term)

    def seal2(implicit ctx: Context): scala.quoted.SealedExpr = {
      type T
      val expr = kernel.QuotedExpr_seal(term).asInstanceOf[scala.quoted.Expr[T]]
      val tpe = kernel.QuotedType_seal(term.tpe).asInstanceOf[scala.quoted.Type[T]]
      scala.quoted.SealedExpr(expr)(tpe)
    }
  }

  implicit class TypeToQuotedAPI(tpe: Type) {
    /** Convert `Type` to an `quoted.Type[_]` */
    def seal(implicit ctx: Context): scala.quoted.Type[_] =
      kernel.QuotedType_seal(tpe)
  }
}
