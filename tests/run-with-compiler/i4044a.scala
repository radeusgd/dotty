import scala.quoted._

class Foo {
  def foo: Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    run {
      val e: Expr[Int] = '{3}
      val q = '{ ${ '{ $e } } }
      '{ println(${q.show.toExpr}) }
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
