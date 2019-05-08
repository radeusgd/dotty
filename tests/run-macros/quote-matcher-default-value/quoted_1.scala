import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

object Macros {

  inline def defaultValueOf[T]: T = ${impl[T]}

  private def impl[T: Type] given Reflection: Expr[T] = {
    the[Type[T]] match {
//      case '[ Int ] => ('{ 0 }).asInstanceOf[Expr[T]]
//      case '[ Double ] => ('{ 0.0 }).asInstanceOf[Expr[T]]
      case '[ List[$t] ] => ('{ List.empty[$t] }).asInstanceOf[Expr[T]]
//      case '[ Null ] => ('{ null }).asInstanceOf[Expr[T]]
//      case '[ AnyRef ] => ('{ null }).asInstanceOf[Expr[T]]
      case _ => '{ ??? }
    }

  }

}
