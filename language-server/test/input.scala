import scala.quoted._
import scala.collection.mutable.ListBuffer

object InputTest {
  inline def hello(expr: => Any) <: Any =
    ${ impl('expr) }

  def impl(expr: Expr[Any])(implicit qctx: QuoteContext) = {
    import qctx.tasty._

    val buffer = new ListBuffer[Literal]

    val traverser = new TreeTraverser {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case IsLiteral(tree) =>
            buffer += tree
          case _ =>
        }
        super.traverseTree(tree)
      }
    }

    val trees = List(expr.unseal.underlyingArgument)
    trees.foreach(traverser.traverseTree)
    //setReferences(buffer.toList.map(_.namePos))
    val lits = buffer.toList
    val inputs = lits.map(lit => s"""
    <input type="range" min="0" max="100" value=${lit.constant.value}>
    <label>${lit.constant.value}</label>
    <br/>
    """)
    val body = s"""
    ${inputs.mkString("\n")}
    """
    setWebview(Webview("Literals", body))

    //Literal(Constant(buffer.toList.toString)).seal
    expr
  }

  def test: Unit = {
    val x = hello({
      def f: Int = 10+1
      f+3
    })
  }
}
