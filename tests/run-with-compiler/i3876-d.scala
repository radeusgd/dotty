import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    val x: Expr[Int] = '{3}

    val f4: Expr[Int => Int] = '{
      inlineLambda
    }

    run {
      val y = f4(x)
      '{
        println($y)
        println(${y.show.toExpr})
      }
    }
  }

  inline def inlineLambda <: Int => Int = x => x + x
}