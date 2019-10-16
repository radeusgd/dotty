import scala.quoted._
import scala.compiletime._

object Macros {

  trait CRig[A: Numeric] {
    def plus(x: A, y: A): A
  }

  inline def genCRig[T](given ev: Numeric[T]): CRig[T] = {
    class SpecCRig extends CRig[T] {
      def plus(x: T, y: T): T = {
        plus2(x, y)(given ev)
      }
    }
    new SpecCRig
  }

  final val intNum = genCRig[Int]
  final val bigIntNum = genCRig[BigDecimal]

  private inline def plusGen[T: Numeric](x: T, y: T): T = summon[Numeric[T]].plus(x, y)
  private inline def plusInt(x: Int, y: Int): Int = x + y

  inline def plus2[T](x: T, y: T)(given ev: Numeric[T]): T = inline erasedValue[T] match {
    case _: Int =>
      plusInt(x.asInstanceOf[Int], y.asInstanceOf[Int]).asInstanceOf[T]
    case _ =>
      summonFrom {
        case given num: Numeric[T] => plusGen(x, y)
        case _ => error("Could not find Numeric instance")
      }
  }
}
