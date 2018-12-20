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


  // support type classes that are polymorphic over a proper type
  def proper(): Unit = {
    // generates an implicit summoning method in companion
    Semigroup[Int] shouldBe Semigroup.semigroupInt

    // generates object oriented style forwarding methods
    {
      import Semigroup.ops._
      1 append 2 shouldBe 3
      1 appendCurried 2 shouldBe 3
    }

    // supports type class inheritance
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

    // supports pre-existing companions
    {
      @typeclass trait Sg[A] {
        def op(x: A, y: A): A
      }
      object Sg {
        def foo = 1
      }
      implicit val sgInt: Sg[Int] = new Sg[Int] {
        def op(x: Int, y: Int) = x + y
      }

      Sg[Int].op(1, 2) shouldBe 3
      Sg.foo shouldBe 1
    }

    // XXX: neg test
    // // supports changing the name of adapted methods
    // {
    //   @typeclass trait Sg[A] {
    //     @op("|+|") def append(x: A, y: A): A
    //   }
    //   implicit val sgInt: Sg[Int] = new Sg[Int] {
    //     def append(x: Int, y: Int) = x + y
    //   }

    //   import Sg.ops._
    //   1 |+| 2 shouldBe 3
    //   "1 append 2" shouldNot compile
    // }

    // supports aliasing the name of adapted methods
    {
      @typeclass trait Sg[A] {
        @op("|+|", alias = true) def append(x: A, y: A): A
      }
      implicit val sgInt: Sg[Int] = new Sg[Int] {
        def append(x: Int, y: Int) = x + y
      }

      import Sg.ops._
      1 |+| 2 shouldBe 3
      1 append 2 shouldBe 3
    }

    // XXX: not implemented
    // // supports aliasing the name of adapted methods (without named arg)
    // {
    //   @typeclass trait Sg[A] {
    //     @op("|+|", true) def append(x: A, y: A): A
    //     @simulacrum.op("~", true) def foo(x: A, y: A): A = append(x, y)
    //   }
    //   implicit val sgInt: Sg[Int] = new Sg[Int] {
    //     def append(x: Int, y: Int) = x + y
    //   }

    //   import Sg.ops._
    //   1 |+| 2 shouldBe 3
    //   1 append 2 shouldBe 3
    //   1 foo 2 shouldBe 3
    //   1 ~ 2 shouldBe 3
    // }


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

    // supports type bounds on type class type param
    {
      trait Upper
      trait Lower extends Upper
      trait Mixin[Y]
      @typeclass trait Sub[X <: Upper] { def id(x: X): X = x }
      @typeclass trait Sup[X >: Lower] { def id(x: X): X = x }
      @typeclass trait Both[X >: Lower <: Upper] { def id(x: X): X = x }
      @typeclass trait Lots[X >: Lower with Mixin[Int] <: Upper] { def id(x: X): X = x }
    }

    // supports diamond inheritance
    {
      @typeclass trait Foo[A] { def op(x: A): A }
      @typeclass trait Bar[A] extends Foo[A] {
        def bar(x: A): A
        override def op(x: A): A = bar(x)
      }
      @typeclass trait Baz[A] extends Foo[A] {
        def baz(x: A): A
        override def op(x: A): A = baz(x)
      }
      @typeclass trait Qux[A] extends Bar[A] with Baz[A] { def qux(x: A): A }
      implicit val qint: Qux[Int] = new Qux[Int] {
        def bar(x: Int) = x
        def baz(x: Int) = -x
        def qux(x: Int) = x * 2
      }
      import Qux.ops._
      // XXX: broken, dotty bug ?
      // 1.op shouldBe -1 // Linearization causes the op override from bar to take precedence
    }

    // XXX: not implemented
    // // "supports type classes that extends traits that are not type classes"
    // {
    //   trait Show[A] { def show(a: A): String }
    //   @typeclass(excludeParents = List("Show")) trait ShowingSemigroup[A] extends Show[A] { def append(x: A, y: A): A }
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
    // // "supports suppressing generation of the AllOps trait and the ops object"
    // {
    //   @typeclass(generateAllOps = false) trait Show[A] { def show(a: A): String }
    //   "trait foo extends Show.AllOps" shouldNot compile
    //   "Show.ops" shouldNot compile
    // }

    // supports universal traits
    {
      @typeclass trait Univseral[A] extends Any {
        def foo: A
      }
    }

    // support importing only the non-inherited ops
    {
      @typeclass trait Foo[A] { def foo(a: A): A }
      @typeclass trait Bar[A] extends Foo[A] { def bar(a: A): A }

      import Bar.nonInheritedOps._
      implicit val intBar: Bar[Int] = new Bar[Int] {
        def foo(a: Int) = -a
        def bar(a: Int) = -a
      }
      5.bar shouldBe -5
      // XXX: neg test
      // "5.foo" shouldNot compile
    }
  }

  // support type classes that are polymorphic over a type constructor
  def tc(): Unit = {
    
  }

  def main(args: Array[String]): Unit = {
    proper()
    tc()
  }
}
