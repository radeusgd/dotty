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
  // support type classes that are polymorphic over a proper type
  def proper(): Unit = {
    // supports changing the name of adapted methods
    {
      @typeclass trait Sg[A] {
        @op("|+|") def append(x: A, y: A): A
      }
      implicit val sgInt: Sg[Int] = new Sg[Int] {
        def append(x: Int, y: Int) = x + y
      }

      import Sg.ops._
      1 append 2 // error
    }



    // XXX: not implemented, neg test
    // // supports suppression of adapter methods
    // {
    //   @typeclass trait Sg[A] {
    //     @noop def append(x: A, y: A): A
    //     @simulacrum.noop def foo(x: A, y: A): A = append(x, y)
    //   }
    //   implicit val sgInt: Sg[Int] = new Sg[Int] {
    //     def append(x: Int, y: Int) = x + y
    //   }

    //   "1 append 2 shouldBe 3" shouldNot compile
    //   "1 foo 2 shouldBe 3" shouldNot compile
    // }

    // XXX: not implemented, neg test
    // // report compilation failures when excludeParents attribute references unknown parent
    // {
    //   trait Show[A] { def show(a: A): String }
    //   """
    //     @typeclass(excludeParents = List("Show", "Foo")) trait ShowingSemigroup[A] extends Show[A] { def append(x: A, y: A): A }
    //     """ shouldNot compile
    // }


    // XXX: not implemented, neg test
    // // supports suppressing generation of the AllOps trait and the ops object
    // {
    //   @typeclass(generateAllOps = false) trait Show[A] { def show(a: A): String }
    //   "trait foo extends Show.AllOps" shouldNot compile
    //   "Show.ops" shouldNot compile
    // }

    // support importing only the non-inherited ops
    {
      @typeclass trait Foo[A] { def foo(a: A): A }
      @typeclass trait Bar[A] extends Foo[A] { def bar(a: A): A }

      import Bar.nonInheritedOps._
      implicit val intBar: Bar[Int] = new Bar[Int] {
        def foo(a: Int) = -a
        def bar(a: Int) = -a
      }
      5.foo // error
    }
  }

  // support type classes that are polymorphic over a type constructor
  def tc(): Unit = {
    // generates object oriented style forwarding methods
    {
      List(1, 2, 3).as(0) // error
      import Functor.ops._
      List(1, 2, 3).as(0) // ok
    }

    // supports changing the name of adapted methods
    {
      @typeclass trait Monad[G[_]] extends Functor[G] {
        def pure[A](a: => A): G[A]
        @op(">>=") def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B]
        override def map[A, B](ga: G[A])(f: A => B) = flatMap(ga) { a => pure(f(a)) }
      }
      implicit val monadList: Monad[List] = new Monad[List] {
        def pure[A](a: => A) = List(a)
        def flatMap[A, B](ga: List[A])(f: A => List[B]): List[B] = ga.flatMap(f)
      }
      import Monad.ops._
      val twice: Int => List[Int] = x => List(x, x)

      Monad.Ops(List(1, 2, 3)) flatMap twice // error
    }
  }
}
