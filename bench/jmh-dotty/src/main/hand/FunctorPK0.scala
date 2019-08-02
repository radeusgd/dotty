import Hand.{Eq, Functor}
  object FunctorPK0 extends Functor[PK0] {
    def map[A, B](fa: PK0[A])(f: A => B): PK0[B] = PK0()
  } // FunctorPK0
