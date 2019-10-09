import scala.quoted._
import scala.collection.mutable.ListBuffer

object SVTest {
  inline def hello(expr: => Any) <: Any =
    ${ impl('expr) }

  def impl(expr: Expr[Any])(implicit qctx: QuoteContext) = {
    import qctx.tasty._

    val buffer = new ListBuffer[Literal]

    val traverser = new TreeAccumulator[ujson.Arr] {
      override def foldTree(arr: ujson.Arr, tree: Tree)(given ctx: Context): ujson.Arr = {
        val node = ujson.Obj()
        arr.arr.append(node)
        node("name") = tree.asInstanceOf[Product].productPrefix.toString
        val children = ujson.Arr()
        node("children") = children
        foldOverTree(children, tree)
        arr
      }
      override def foldPattern(arr: ujson.Arr, tree: Pattern)(given ctx: Context): ujson.Arr = arr
    }

    val json = traverser.foldTree(ujson.Arr(), expr.unseal.underlyingArgument)(0)
    val body = s"""
    <style>
    svg {
      background-color: white;
    }

    .node {
      fill: steelblue;
      stroke: none;
    }

    .link {
      fill: none;
      stroke: #ccc;
      stroke-width: 1px;
    }
    </style>
    <script src="https://d3js.org/d3.v5.min.js"></script>

    <svg width="500" height="1000" id="svg">
      <g transform="translate(5, 5)">
        <g class="links"></g>
        <g class="nodes"></g>
      </g>
    </svg>

    <script>
    var data = ${ujson.write(json)};
    var root = d3.hierarchy(data);
    var tree = d3.tree().size([500, 600]);
    tree(root);

    // Nodes
    const nodes = d3.select('svg g.nodes')
      .selectAll('circle.node')
      .data(root.descendants())
      .enter()
      .append('g')


    nodes.append('circle')
      .classed('node', true)
      .attr('cx', function(d) {return d.x;})
      .attr('cy', function(d) {return d.y;})
      .attr('r', 4)

    nodes.append('text')
      .attr('x', function(d) {return d.x+10;})
      .attr('y', function(d) {return d.y+10;})
      .text(d => d.data.name)

    // Links
    d3.select('svg g.links')
      .selectAll('line.link')
      .data(root.links())
      .enter()
      .append('line')
      .classed('link', true)
      .attr('x1', function(d) {return d.source.x;})
      .attr('y1', function(d) {return d.source.y;})
      .attr('x2', function(d) {return d.target.x;})
      .attr('y2', function(d) {return d.target.y;});
    </script>
    """

    setWebview(Webview("Literals", body))

    Literal(Constant(body)).seal
    //expr
    // Literal(Constant(ujson.write(json))).seal
  }

  def test: Unit = {
    val x = hello({
      List(1,2,3)
    })
  }
}
