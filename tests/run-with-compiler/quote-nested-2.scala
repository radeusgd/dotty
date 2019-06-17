import quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = run {
    val q = '{
      val a = '{4}
      '{${a}}
    }

    println(q.show)
    '{}
  }
}
