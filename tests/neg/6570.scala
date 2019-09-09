object Base {
  trait Trait1
  trait Trait2
  type N[t] = t match {
    case String => Trait1
    case Int => Trait2
  }
}
import Base._

object UpperBoundParametricVariant {
  trait Cov[+T]
  type M[t] = t match {
    case Cov[x] => N[x]
  }

  trait Root[A] {
    def thing: M[A]
  }

  trait Child[A <: Cov[Int]] extends Root[A]

  // we reduce `M[T]` to `Trait2`, even though we cannot be certain of that
  def foo[T <: Cov[Int]](c: Child[T]): Trait2 = c.thing // error

  // class Asploder extends Child[Cov[String & Int]] {
  //   def thing = new Trait1 {}
  // }

  // def explode = foo(new Asploder) // ClassCastException
}
