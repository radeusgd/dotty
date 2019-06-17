import quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    val q = '{ '{3} }
    println(q.show)
    '{}
  }
}
