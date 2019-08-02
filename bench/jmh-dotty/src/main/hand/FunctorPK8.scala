import Hand.{Eq, Functor}
  object FunctorPK8 extends Functor[PK8] {
    def map[A, B](fa: PK8[A])(f: A => B): PK8[B] = PK8[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0))
  } // FunctorPK8
