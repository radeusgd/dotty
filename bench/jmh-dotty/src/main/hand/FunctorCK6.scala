import Hand.{Eq, Functor}
  object FunctorCK6 extends Functor[CK6] {
    import CK6._
    def map[A, B](fa: CK6[A])(f: A => B): CK6[B] = fa match {
      case A0(x) => A0(f(x))
      case B0(x) => B0(x)
      case S0(x) => S0(f(x))
      case D0(x) => D0(x)
      case E0(x) => E0(f(x))
      case F0(x) => F0(x)
    }
  } // FunctorCK6
