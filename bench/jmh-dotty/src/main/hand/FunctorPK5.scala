import Hand.{Eq, Functor}
  object FunctorPK5 extends Functor[PK5] {
    def map[A, B](fa: PK5[A])(f: A => B): PK5[B] = PK5[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0)
  } // FunctorPK5
