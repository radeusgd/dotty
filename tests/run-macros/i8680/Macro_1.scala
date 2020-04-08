import scala.quoted._

object Macros {
  inline def testSeq(): Any = ${impl('{java.util.Arrays.asList[Int](Seq(1): _*)})}
  inline def testArray(): Any = ${impl('{java.util.Arrays.asList[Int](Array(1): _*)})}

  private def impl(expr: Expr[Any])(using QuoteContext): Expr[Any] = expr match {
    case '{ java.util.Arrays.asList[$t]($xs: _*) } =>
      xs
    case _ =>
      '{ "FAIL" }
  }
}
