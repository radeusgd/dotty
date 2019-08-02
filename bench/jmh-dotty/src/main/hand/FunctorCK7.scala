import Hand.{Eq, Functor}
  object FunctorCK7 extends Functor[CK7] {
    import CK7._
    def map[A, B](fa: CK7[A])(f: A => B): CK7[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x))
    }
  } // FunctorCK7
