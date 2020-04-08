import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    println(testSeq()) // "List(1)"
    println(testArray()) // "FAIL"
  }
}
