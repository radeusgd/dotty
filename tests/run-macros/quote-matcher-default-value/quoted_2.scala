import Macros._

object Test {

  def main(args: Array[String]): Unit = {
    println(defaultValueOf[Int])
    println(defaultValueOf[Double])
  }

}
