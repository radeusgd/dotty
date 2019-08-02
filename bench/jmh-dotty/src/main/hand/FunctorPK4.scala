import Hand.{Eq, Functor}
  object FunctorPK4 extends Functor[PK4] {
    def map[A, B](fa: PK4[A])(f: A => B): PK4[B] = PK4[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0))
  } // FunctorPK4
