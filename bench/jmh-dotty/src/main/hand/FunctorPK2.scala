import Hand.{Eq, Functor}
  object FunctorPK2 extends Functor[PK2] {
    def map[A, B](fa: PK2[A])(f: A => B): PK2[B] = PK2[B](fa.a0, f(fa.b0))
  } // FunctorPK2
