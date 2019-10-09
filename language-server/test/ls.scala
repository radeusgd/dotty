import scala.quoted._
import scala.collection.mutable.ListBuffer

object LSTest {
  inline def hello(expr: => Any) <: Any =
    ${ impl('expr) }

  def impl(expr: Expr[Any])(implicit qctx: QuoteContext) = {
    import qctx.tasty._

    val buffer = new ListBuffer[Tree]

    val traverser = new TreeTraverser {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case IsDefinition(definition) if definition.name.startsWith("h") =>
            buffer += definition
          case _ =>
        }
        super.traverseTree(tree)
      }
    }

    val trees = projectTrees
    trees.foreach(traverser.traverseTree)
    setReferences(buffer.toList.map(_.namePos))

    expr
  }

  def test: Unit = {
    val x = hello({
      def f: Int = 1
      f
    })
  }
}
