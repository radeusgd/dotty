import scala.quoted._

object Test {
  object UTensorOps {

    type Shape = Array[Int]

    class UTensor(val shape: Shape, val data: Expr[Array[Double]]) {

    }

    def add(t: UTensor, v: UTensor): given QuoteContext => UTensor = {
      val sizeExpr = t.shape.fold(1)(_ * _).toExpr
      '{
        var i = 0
        val arr = new Array[Double]($sizeExpr)
        while (i < $sizeExpr) {
          arr(i) = $t(i) + $v(i)
        }
        arr
      }
    }

    def mult(t: UTensor, v: UTensor): given QuoteContext => UTensor = {
      val sizeExpr = shape.fold(1)(_ * _).toExpr
      '{
        var i = 0
        val arr = new Array[Double]($sizeExpr)
        while (i < $sizeExpr) {
          arr(i) = $t(i) * $v(i)
        }
        arr
      }
    }
  }


  def main(args:Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    def add given QuoteContext = (shape: Shape) => '{  (t1: Array[Double], t2: Array[Double]) => ${ UTensorOps.add(shape, 't1, 't2) } }
    def mult given QuoteContext = (shape: Shape, k: Int) => '{  (t1: Array[Double], t2: Array[Double]) => ${ UTensorOps.mult(shape, 't1, 't2) } }

    withQuoteContext {
      println(add(Array(2, 3)).show)
    }
  }

}