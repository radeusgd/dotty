package dotty.tools.dotc.transform

import java.net.URLClassLoader

import dotty.meta.TastyString
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.tasty.Quotes._

/** TODO
 */
class Splice extends MiniPhase {
  import tpd._

  override def phaseName: String = "splice"

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context): tpd.Tree = {
    tree.fun match {
      case fun: TypeApply if fun.symbol eq defn.MetaSplice =>
        def splice(str: String): Tree = {
          val splicedCode = revealQuote(unpickle(new dotty.meta.Expr(str, Nil).tasty)) // TODO add args
          transformAllDeep(splicedCode)
        }
        tree.args.head match {
          // splice explicit quotes
          case Apply(TypeApply(_, _), List(Literal(Constant(tastyStr: String)))) => splice(tastyStr)
          case Inlined(_, _, Apply(TypeApply(_, _), List(Literal(Constant(tastyStr: String))))) => splice(tastyStr)

          // splice quotes with code ~'x --> x
          case Apply(quote, arg :: Nil) if quote.symbol == defn.MetaQuote => arg
          case Inlined(_, _, Apply(quote, arg :: Nil)) if quote.symbol == defn.MetaQuote => arg

          case quote: Apply => reflectQuote(quote)
          case quote: RefTree => reflectQuote(quote)
          case _ => tree
        }
      case _ => tree
    }
  }

  private def reflectQuote(tree: Tree)(implicit ctx: Context): Tree = {
    if (!tree.tpe.derivesFrom(defn.MetaExpr) ||
        tree.symbol == defn.MetaQuote ||
        tree.symbol.isConstructor)
      return tree
    val sym = tree.symbol
    try {
      val urls = ctx.settings.classpath.value.split(':').map(cp => java.nio.file.Paths.get(cp).toUri.toURL)
      val classLoader = new URLClassLoader(urls, getClass.getClassLoader)
      val clazz = classLoader.loadClass(sym.owner.showFullName)
      val args: List[AnyRef] = tree match {
        case Apply(_, args) =>
          args.map {
            case arg @ Apply(_, quote :: Nil) if arg.symbol eq defn.MetaQuote =>
              val tasty = pickle(encapsulateQuote(quote))
              val tastyString = TastyString.tastyToString(tasty)
              new dotty.meta.Expr(tastyString, Nil) // TODO add args?
            case Literal(Constant(c)) => c.asInstanceOf[AnyRef]
            case arg @ Apply(Select(Apply(_, quote :: Nil), _), Apply(_, splice :: Nil) :: Nil) if arg.symbol eq defn.MetaExprSpliced =>
              val tasty = pickle(encapsulateQuote(quote))
              val tastyString = TastyString.tastyToString(tasty)


              println()
              println(splice.show)
              println(splice)
              println()
              println(encapsulateQuote(splice).show)
              println()
              println()
              val tasty2 = pickle(encapsulateQuote(splice))
              val tastyString2 = TastyString.tastyToString(tasty2)


              val spliceExpr = new dotty.meta.Expr(tastyString2, Nil)
              // TODO Support multiple expr
              new dotty.meta.Expr(tastyString, Nil).spliced(spliceExpr)
            case arg => ???
          }
        case _ => Nil
      }
      val paramClasses = sym.signature.paramsSig.map { param =>
        if (param.toString == "scala.Int") 0.getClass // TODO
        else classLoader.loadClass(param.toString)
      }
      val method = clazz.getDeclaredMethod(sym.name.toString, paramClasses: _*)
      val expr = method.invoke(null, args: _*).asInstanceOf[dotty.meta.Expr[_]]

      foo(expr)
    } catch {
      case _: NoSuchMethodException =>
        ctx.error(s"Could not find macro ${sym.showFullName} in classpath", tree.pos)
        tree
      case _: ClassNotFoundException =>
        ctx.error(s"Could not find macro class ${sym.owner.showFullName} in classpath", tree.pos)
        tree
    }
  }

  private def foo(expr: dotty.meta.Expr[_])(implicit ctx: Context): Tree = {
    val splices = expr.splices.map(foo)
    val code = revealQuote(unpickle(expr.tasty))
    spliceIn(code, splices)

  }

  private def spliceIn(code: Tree, splices: List[Tree])(implicit ctx: Context): Tree = {
    if (splices.isEmpty) code
    else {
      new TreeMap() {
        private[this] var splicesLeft = splices
        override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
          splicesLeft match {
            case splice :: rest =>
              if (tree.symbol eq defn.MetaSpliceHole) {
                splicesLeft = rest
                splice
              }
              else super.transform(tree)
            case Nil => tree
          }
        }
      }.transform(code)
    }
  }

}
