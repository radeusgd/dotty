import Hand.{Eq, Functor}
  object FunctorCK5 extends Functor[CK5] {
    import CK5._
    def map[A, B](fa: CK5[A])(f: A => B): CK5[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x))
    }
  } // FunctorCK5
