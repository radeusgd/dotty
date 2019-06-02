import scala.collection.mutable.WrappedArray
import scala.compiletime._
import scala.deriving._

// Standard library

object Utils {
  type Id[t] = t
  type Const[c] = [t] =>> c
  case class Wrap[T](t: T)


  type ~>[A[_], B[_]] = [t] => A[t] => B[t]

  inline def summon[T] = implicit match {
    case t: T => t
  }

  inline def summonValues[T] <: Tuple = inline erasedValue[T] match {
    case _: Unit => ()
    case _: (a *: b) => constValue[a] *: summonValues[b]
  }

  inline def summonValuesAsArray[T]: Array[Any] = inline erasedValue[Id[T]] match {
    case _: Unit => Array()
    case _: Tuple1[a] => Array(constValue[a])
    case _: (a, b) => Array(constValue[a], constValue[b])
    case _: (a, b, c) => Array(constValue[a], constValue[b], constValue[c])
    case _: (a, b, c, d) => Array(constValue[a], constValue[b], constValue[c], constValue[d])
    case _: (a, b, c, d, e) => Array(constValue[a], constValue[b], constValue[c], constValue[d], constValue[e])
    // Add fallback for larger sizes
  }

  case class Fix[S[_, _], A](unfix: S[A, Fix[S, A]])
}

case class Labelling[T](label: String, elemLabels: Seq[String])
object Labelling {
  inline implicit def apply[T0](implicit mirror: Mirror { type MirroredType = T0 }): Labelling[T0] =
    Labelling[T0](constValue[mirror.MirroredLabel & String], WrappedArray.make[String](Utils.summonValuesAsArray[mirror.MirroredElemLabels]))
}
