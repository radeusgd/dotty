import scala.quoted._
import scala.collection.mutable.ListBuffer
//buuuuuuuuuii//hiii
object Input2Test {
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
    <input type="range" min="0" max="100" value=${lit.constant.value}
      oninput="update(this)"
      data-start-line="${lit.pos.startLine}"
      data-start-column="${lit.pos.startColumn}"
      data-end-line="${lit.pos.endLine}"
      data-end-column="${lit.pos.endColumn}"
      >
    <label>${lit.constant.value}</label>
    <br/>
    """)
    val body = s"""
    ${inputs.mkString("\n")}
    <script>
    const vscode = acquireVsCodeApi();

    function update(input) {
      //console.log(input.dataset.startPos)
      vscode.postMessage({
        command: "applyEdit",
        value: input.value,
        startLine: input.dataset.startLine,
        startColumn: input.dataset.startColumn,
        endLine: input.dataset.endLine,
        endColumn: input.dataset.endColumn
      })
    }
    </script>
    """
    setWebview(Webview("Literals", body))

    //Literal(Constant(buffer.toList.toString)).seal
    expr
  }

  def test: Unit = {
    val x = hello({
      def f: Int = 10+1
      f+26
    })
  }
}
