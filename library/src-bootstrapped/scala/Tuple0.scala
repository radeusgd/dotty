package scala

class Tuple0 extends Tuple with Product0 {
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Tuple0]
  override def toString: String = "()"
}

object Tuple0 extends Tuple0 {
  def apply(): Tuple0 = this
  def unapply(arg: Tuple0): Boolean = true
}
