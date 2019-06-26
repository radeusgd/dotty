import Macros._
object Test {
  def main(args: Array[String]): Unit = {
    val x = 1
    printBinOpArgs(x != 0)
    printBinOpArgs(x == 0)
  }
}
