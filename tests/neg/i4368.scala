object Test1 {
  trait X {
    type A = B
    type B
  }
  trait Y {
    type A
    type B = A
  }
  trait Z extends X with Y // error: cyclic
}

object Test2 {
  trait W {
    type A
    type B
  }
  trait X { z: W =>
    type A = z.B
    type B
  }
  trait Y { z: W =>
    type A
    type B = z.A
  }
  trait Z extends X with Y // error: cyclic
}

object Test3 {
  trait W {
    type A
    type B
  }
  trait X { z: W =>
    type A = z.B
    type B
  }
  trait Y { z: W =>
    type A
    type B = z.A
  }

  object App {
    type Z = X with Y
    val z: Z = z
    val a: z.A = a // error: too deep
  }
}

object Test4 {
  trait X[F[_]] {
    protected type A = F[B]
    protected type B
  }
  trait Y[F[_]] {
    protected type A
    protected type B = F[A]
  }

  trait Fix[F[_]] extends X[F] with Y[F] {
    type Result = A    // error: too deep
  }
}

object Test5 {
  trait X {
    type A = B
    type B
  }
  trait Y {
    type A
    type B = A
  }

  object App {
    type Z = X & Y
    val z: Z = z
    val a: z.A = a  // error: too deep
  }
}

object Test6 {
  trait W { type T <: W; val t: T }
  trait X {
    type A = b.T
    val a : A = b.t
    type B <: W
    val b : B
  }
  trait Y {
    type A <: W
    val a : A
    type B = a.T
    val b = a.t
  }
  trait Z extends X with Y // error: cyclic
}

object Test7 {
  class Fix[F[_]] {
    class Foo { type R >: F[T] <: F[T] } // error: cyclic
    type T = F[Foo#R]
  }

  object App {
    type Nat = Fix[Option]#T
  }
}
/*
object Test8 {

  class A {
    type T = B#U
  }

  class B {
    type U = A#T
  }
}
*/
object Test9 {
  trait W {
    type A
  }
  trait X extends W {
    type A = B
    type B
  }
  trait Y extends W {
    type A
    type B = A
  }

  trait Foo[X <: W, Y <: W] {
    type Z = X & Y
    val z: Z
    val a: z.A
  }

  trait Boo {
    val f: Foo[X, Y]
  }

  trait Baz extends Boo {
    val a = f.a // error: member search too deep
      // this should be a cyclic error, but it goes undetected
      // scalac reports a volatility error, but the dotty equivalent (checkRealizable)
      // is checked too late.
  }
}