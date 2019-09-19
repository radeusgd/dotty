
import scala.quoted.{_, given}
import scala.quoted.staging._

class Foo[T: Type] {
  def q(given QuoteContext) = '{(null: Any).asInstanceOf[T]}
}

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    println((new Foo[Object]).q.show)
    println((new Foo[String]).q.show)
  }
}
