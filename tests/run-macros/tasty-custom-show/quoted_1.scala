import scala.quoted.{_, given}
import scala.quoted.autolift.given


object Macros {

  implicit inline def printOwners[T](x: => T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(given qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._

    val buff = new StringBuilder

    val output = new TreeTraverser {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        // Use custom Show[_] here
        val printer = dummyShow
        tree match {
          case IsDefinition(tree @ DefDef(name, _, _, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(printer.showTree(tree))
            buff.append("\n\n")
          case IsDefinition(tree @ ValDef(name, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(printer.showTree(tree))
            buff.append("\n\n")
          case _ =>
        }
        traverseTreeChildren(tree)
      }
    }

    val tree = x.unseal
    output.traverseTree(tree)
    '{print(${buff.result()})}
  }

  def dummyShow(given qctx: QuoteContext): qctx.tasty.Printer = {
    import qctx.tasty._
    new Printer {
      def showTree(tree: Tree)(implicit ctx: Context): String = "Tree"
      def showPattern(pattern: Pattern)(implicit ctx: Context): String = "Pattern"
      def showTypeOrBounds(tpe: TypeOrBounds)(implicit ctx: Context): String = "TypeOrBounds"
      def showConstant(const: Constant)(implicit ctx: Context): String = "Constant"
      def showSymbol(symbol: Symbol)(implicit ctx: Context): String = "Symbol"
      def showFlags(flags: Flags)(implicit ctx: Context): String = "Flags"
    }
  }

}
