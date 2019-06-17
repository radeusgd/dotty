package dotty.tools.dotc
package quoted

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.quoted.PickledQuotes

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find implicit scala.quoted.QuoteContext. TODO")
class QuoteContext(ctx: Context) extends scala.quoted.QuoteContext {

  def show[T](expr: scala.quoted.Expr[T]): String =
    doShow(PickledQuotes.quotedExprToTree(expr)(ctx), ctx)

  def show[T](tpe: scala.quoted.Type[T]): String =
    doShow(PickledQuotes.quotedTypeToTree(tpe)(ctx), ctx)

  private def doShow(tree: Tree, ctx: Context): String = {
    implicit val c: Context = ctx
    val tree1 =
      if (ctx.settings.YshowRawQuoteTrees.value) tree
      else (new TreeCleaner).transform(tree)
    tastyreflect.ReflectionImpl.showTree(tree1)
  }
}
