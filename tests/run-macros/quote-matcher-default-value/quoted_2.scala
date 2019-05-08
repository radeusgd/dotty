import Macros._

object Test {

  def main(args: Array[String]): Unit = {
    type T
//    println(defaultValueOf[Int])
//    println(defaultValueOf[Double])
    println(defaultValueOf[List[Int]])
//    println(defaultValueOf[Null])
//    println(defaultValueOf[AnyRef])
//    println(defaultValueOf[AnyRef])
    println(defaultValueOf[List[T]])
  }

}
