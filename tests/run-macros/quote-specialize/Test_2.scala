import scala.compiletime.{erasedValue, error}
import Macros._

@main def Test = {

  plus2(1, 2)

  plus2(BigDecimal(1), BigDecimal(2))
}

