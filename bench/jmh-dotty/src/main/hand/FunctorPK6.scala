import Hand.{Eq, Functor}
  object FunctorPK6 extends Functor[PK6] {
    def map[A, B](fa: PK6[A])(f: A => B): PK6[B] = PK6[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0))
  } // FunctorPK6
