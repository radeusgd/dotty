import Hand.{Eq, Functor}
  object FunctorCK2 extends Functor[CK2] {
    import CK2._
    def map[A, B](fa: CK2[A])(f: A => B): CK2[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x)
    }
  } // FunctorCK2
