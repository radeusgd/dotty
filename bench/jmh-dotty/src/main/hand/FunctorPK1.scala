import Hand.{Eq, Functor}
  object FunctorPK1 extends Functor[PK1] {
    def map[A, B](fa: PK1[A])(f: A => B): PK1[B] = PK1[B](fa.a0)
  } // FunctorPK1
