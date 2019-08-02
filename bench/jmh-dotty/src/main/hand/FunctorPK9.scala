import Hand.{Eq, Functor}
  object FunctorPK9 extends Functor[PK9] {
    def map[A, B](fa: PK9[A])(f: A => B): PK9[B] = PK9[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0)
  } // FunctorPK9
