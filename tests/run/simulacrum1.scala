// Adapted from https://github.com/mpilquist/simulacrum/blob/master/core/src/test/scala/simulacrum/typeclass.scala

import simulacrum._

@typeclass trait Semigroup[T] {
  @op("|+|", alias = true)
  def append(x: T, y: T): T
  def appendCurried(x: T)(y: T): T = append(x, y)
}

object Semigroup {
  implicit val semigroupInt: Semigroup[Int] = new Semigroup[Int] {
    def append(x: Int, y: Int) = x + y
  }
}

@typeclass trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def as[A, B](fa: F[A], b: => B): F[B] = map(fa)(_ => b)
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
  def foo[G[_], A](fga: F[G[A]]): G[F[A]] = ???
}

object Functor {
  implicit val functorList: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
  }
}

object Test {
  implicit class ShouldBe[T](x: T) {
    def shouldBe(y: T): Unit =
      assert(x == y)
  }


  def main(args: Array[String]): Unit = {
    Semigroup[Int] shouldBe Semigroup.semigroupInt

    {
      import Semigroup.ops._
      1 append 2 shouldBe 3
      1 appendCurried 2 shouldBe 3
    }

    {
      @typeclass trait Monoid[X] extends Semigroup[X] {
        def id: X
      }
      implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
        def append(x: Int, y: Int) = x + y
        def id = 0
      }
      Monoid[Int].id shouldBe 0
      Monoid[Int].append(1, 2) shouldBe 3
      import Monoid.ops._
      1 append 2 shouldBe 3
    }
  }
}
