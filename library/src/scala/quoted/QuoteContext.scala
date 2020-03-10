package scala.quoted

import scala.quoted.show.SyntaxHighlight

/** Quotation context provided by a macro expansion or in the scope of `scala.quoted.run`.
 *  Used to perform all operations on quoted `Expr` or `Type`.
 *
 *  It contains the low-level Typed AST API `tasty` meta-programming API.
 *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
 *
 *  @param tasty Typed AST API. Usage: `def f(qctx: QuoteContext) = { import qctx.tasty._; ... }`.
 */
class QuoteContext(val tasty: scala.tasty.Reflection) extends QuoteCtx { self =>

  type Expr[+T] = scala.quoted.Expr[T]

  /** Report an error at the position of the macro expansion */
  def error(msg: => String): Unit =
    tasty.error(msg, tasty.rootPosition)

  /** Report an error at the on the position of `expr` */
  def error(msg: => String, expr: Expr[Any]): Unit =
    tasty.error(msg, expr.unseal(using this).pos)

  /** Report an error at the position of the macro expansion and throws a StopQuotedContext */
  def throwError(msg: => String): Nothing = {
    error(msg)
    throw new StopQuotedContext
  }
  /** Report an error at the on the position of `expr` and throws a StopQuotedContext */
  def throwError(msg: => String, expr: Expr[Any]): Nothing = {
    error(msg, expr)
    throw new StopQuotedContext
  }

  /** Report a warning */
  def warning(msg: => String): Unit =
    tasty.warning(msg, tasty.rootPosition)

  /** Report a warning at the on the position of `expr` */
  def warning(msg: => String, expr: Expr[_]): Unit =
    tasty.warning(msg, expr.unseal(using this).pos)

}

trait QuoteCtx { self =>
  val tasty: scala.tasty.Reflection

  // TODO add docs
  type Expr[+T]
  type Type[T]

  final def [T](expr: Expr[T]).value(unlift: Unliftable[T]): T =
    ???
    // valueOf(expr).getOrElse(qctx.throwError(s"Expected a known value. \n\nThe value of: $show\ncould not be recovered using $valueOf", expr))

  // extension on [T](expr: Expr[T]) {
    //  /** Show a source code like representation of this expression without syntax highlight */
    // def show(implicit qctx: QuoteContext): String = qctx.show(this, SyntaxHighlight.plain)

    // /** Show a source code like representation of this expression */
    // def show(syntaxHighlight: SyntaxHighlight)(implicit qctx: QuoteContext): String = qctx.show(this, syntaxHighlight)

    // /** Return the value of this expression.
    // *
    // *  Returns `None` if the expression does not contain a value or contains side effects.
    // *  Otherwise returns the `Some` of the value.
    // */
    // final def getValue[U >: T](using qctx: QuoteContext, valueOf: ValueOfExpr[U]): Option[U] = valueOf(this)

    /** Return the value of this expression.
     *
     *  Emits an error error and throws if the expression does not contain a value or contains side effects.
     *  Otherwise returns the value.
     */
    // final def value(valueOf: ValueOfExpr[T]): T = ???
      // valueOf(expr).getOrElse(qctx.throwError(s"Expected a known value. \n\nThe value of: $show\ncould not be recovered using $valueOf", expr))

    // /** Pattern matches `this` against `that`. Effectively performing a deep equality check.
    // *  It does the equivalent of
    // *  ```
    // *  this match
    // *    case '{...} => true // where the contens of the pattern are the contents of `that`
    // *    case _ => false
    // *  ```
    // */
    // final def matches(that: Expr[Any])(using qctx: QuoteContext): Boolean =
    //   !scala.internal.quoted.Expr.unapply[Unit, Unit](this)(using that, false, qctx).isEmpty

    // /** Checked cast to a `quoted.Expr[U]` */
    // def cast[U](using tp: scala.quoted.Type[U])(using qctx: QuoteContext): scala.quoted.Expr[U] =
    //   qctx.tasty.internal.QuotedExpr_cast[U](this)(using tp, qctx.tasty.rootContext)

  /** View this expression `quoted.Expr[T]` as a `Term` */
  def [T](expr: Expr[T]).unseal : self.tasty.Term =
    self.tasty.internal.QuotedExpr_unseal(expr.asInstanceOf[scala.quoted.Expr[_]])(using self.tasty.rootContext)
  // }



  /** Show the fully elaborated source code representation of an expression */
  def (expr: Expr[Any]).show(syntaxHighlight: SyntaxHighlight): String = {
    import tasty.{_, given _}
    // expr.unseal(using this).showWith(syntaxHighlight)
    ???
  }



  object Type {
    /** View this expression `quoted.Type[T]` as a `TypeTree` */
    def [T](tpt: Type[T]).unseal: self.tasty.TypeTree =
      self.tasty.internal.QuotedType_unseal(tpt.asInstanceOf[scala.quoted.Type[_]])(using self.tasty.rootContext)

    /** Show the fully elaborated source code representation of a type */
    def [T](tpe: Type[T]).show(syntaxHighlight: SyntaxHighlight): String = {
      import tasty.{_, given _}
      // tpe.unseal(using this).showWith(syntaxHighlight)
      ???
    }
  }

  /** Type of a QuoteContext profided by a splice within a quote that took this context.
  *  It is only required if working with the reflection API.
  *
  *  Usually it is infered by the quotes an splices typing. But sometimes it is necessary
  *  to explicitly state that a context is nested as in the following example:
  *
  *  ```scala
  *  def run(using qctx: QuoteContext)(tree: qctx.tasty.Tree): Unit =
  *    def nested()(using qctx.NestedContext): Expr[Int] = '{  ${ makeExpr(tree) } + 1  }
  *    '{  ${ nested() } + 2 }
  *  def makeExpr(using qctx: QuoteContext)(tree: qctx.tasty.Tree): Expr[Int] = ???
  *  ```
  */
  type NestedContext = QuoteCtx {
    type Expr[+T] >: self.Expr[T]
    type Type[T] >: self.Type[T]
    val tasty: self.tasty.type
  }

}
