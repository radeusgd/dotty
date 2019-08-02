import Hand.{Eq, Functor}
  object FunctorPK70 extends Functor[PK70] {
    def map[A, B](fa: PK70[A])(f: A => B): PK70[B] =
      PK70[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1),
        fa.a2, f(fa.b2), fa.c2, f(fa.d2), fa.e2, f(fa.f2), fa.g2, f(fa.h2), fa.i2, f(fa.j2),
        fa.a3, f(fa.b3), fa.c3, f(fa.d3), fa.e3, f(fa.f3), fa.g3, f(fa.h3), fa.i3, f(fa.j3),
        fa.a4, f(fa.b4), fa.c4, f(fa.d4), fa.e4, f(fa.f4), fa.g4, f(fa.h4), fa.i4, f(fa.j4),
        fa.a5, f(fa.b5), fa.c5, f(fa.d5), fa.e5, f(fa.f5), fa.g5, f(fa.h5), fa.i5, f(fa.j5),
        fa.a6, f(fa.b6), fa.c6, f(fa.d6), fa.e6, f(fa.f6), fa.g6, f(fa.h6), fa.i6, f(fa.j6)
      )
  } // FunctorPK70
