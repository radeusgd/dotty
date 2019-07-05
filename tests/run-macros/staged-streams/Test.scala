import scala.quoted._

class Test {
  inline def test(f: Int) = ${
    Test.testImpl('{f}) // splice outside quotes
  }
}

object Test {
  def testImpl(f: Expr[Int]): Expr[Int] = f
}