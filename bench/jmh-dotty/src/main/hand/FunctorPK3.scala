import Hand.{Eq, Functor}
  object FunctorPK3 extends Functor[PK3] {
    def map[A, B](fa: PK3[A])(f: A => B): PK3[B] = PK3[B](fa.a0, f(fa.b0), fa.c0)
  } // FunctorPK3
