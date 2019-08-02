import Hand.{Eq, Functor}
  object FunctorPK30 extends Functor[PK30] {
    def map[A, B](fa: PK30[A])(f: A => B): PK30[B] =
      PK30[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1),
        fa.a2, f(fa.b2), fa.c2, f(fa.d2), fa.e2, f(fa.f2), fa.g2, f(fa.h2), fa.i2, f(fa.j2)
      )
  } // FunctorPK30
