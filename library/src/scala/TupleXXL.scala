package scala
import java.util.Arrays.{deepEquals, deepHashCode}

final class TupleXXL private (es: Array[Object]) {
  override def toString = elems.mkString("(", ",", ")")
  override def hashCode = getClass.hashCode * 41 + deepHashCode(elems)
  override def equals(that: Any) = that match {
    case that: TupleXXL => deepEquals(this.elems, that.elems)
    case _ => false
  }
  def elems: Array[Object] = es
}
object TupleXXL {
  def apply(elems: Array[Object]) = new TupleXXL(elems.clone)
}
