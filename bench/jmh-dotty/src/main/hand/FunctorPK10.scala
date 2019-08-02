import Hand.{Eq, Functor}
  object FunctorPK10 extends Functor[PK10] {
    def map[A, B](fa: PK10[A])(f: A => B): PK10[B] =
      PK10[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0)
      )
  } // FunctorPK10
