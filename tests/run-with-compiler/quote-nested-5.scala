import quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = {
    println(show('{
      val a = '{4}
      ${'{
        '{${a}}
      }}
    }))
  }
}
