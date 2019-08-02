import Hand.{Eq, Functor}
  object FunctorPK7 extends Functor[PK7] {
    def map[A, B](fa: PK7[A])(f: A => B): PK7[B] = PK7[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0)
  } // FunctorPK7
