import scala.quoted._
import scala.quoted.matching.BinaryOp
object Macros {

  inline def printBinOpArgs[T](expr: => T): Unit =  ${ impl('expr) }

  private def impl[T: Type](expr: Expr[T]) given tasty.Reflection: Expr[Unit] = {
      expr match {
        case BinaryOp('{ $x: $X }, '{ $y: $Y }, name, op) =>

          '{
            val xx = $x
            val yy = $y
            val res: T = ${ op(x, y) }
            println("arg1: " + xx)
            println("arg2: " + yy)
            println("result: " res)
            println()
          }
        case _ =>
          '{
            val res = $expr
            println("not bin-opp")
            println("result: " res)
          }
      }
  }
}
