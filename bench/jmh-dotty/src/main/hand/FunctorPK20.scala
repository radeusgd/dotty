import Hand.{Eq, Functor}
  object FunctorPK20 extends Functor[PK20] {
    def map[A, B](fa: PK20[A])(f: A => B): PK20[B] =
      PK20[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1)
      )
  } // FunctorPK20
