package tc {
  trait Functor[F[_]]
}

package object tc {
  trait Foo[A]
  type Id[A] = A

  implicit val fooFunctor: Functor[Foo] = new Functor[Foo] {}
  implicit val idFunctor: Functor[Id] = new Functor[Id] {}
}

package test {
  object Test {
    val functor1 = implicitly[tc.Functor[tc.Foo]]
    val functor2 = implicitly[tc.Functor[tc.Id]]  // error
  }
}