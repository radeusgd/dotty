import scala.quoted.{_, given}

object Test {

  implicit def ListIsLiftableAnd[T: Type, U: Type]: Liftable[List[T & U]] = new {
    def toExpr(xs: List[T & U]) = '{ Nil: List[T & U] }
  }

}
