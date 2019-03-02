import simulacrum._

@typeclass trait Semigroup[A] {
  @op("|+|") def append(x: A, y: A): A
}
@typeclass trait Monoid[A] extends Semigroup[A] {
  def id: A
}

object IntMonoids {
  implicit val Additive: Monoid[Int] = new Monoid[Int] {
    def id = 0
    def append(x: Int, y: Int) = x + y
  }

  implicit val Multiplicative: Monoid[Int] = new Monoid[Int] {
    def id = 1
    def append(x: Int, y: Int) = x * y
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    {
      import IntMonoids.Additive
      assert(Monoid[Int] == IntMonoids.Additive)
      assert(Semigroup[Int] == IntMonoids.Additive)

      import Monoid.ops._
      assert((1 |+| 2) == 3)
    }

    {
      import IntMonoids.Multiplicative
      assert(Monoid[Int] == IntMonoids.Multiplicative)
      assert(Semigroup[Int] == IntMonoids.Multiplicative)

      import Monoid.ops._
      assert((1 |+| 2) == 2)
    }
  }
}
