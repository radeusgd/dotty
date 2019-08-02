import Hand.{Eq, Functor}
  object FunctorCK9 extends Functor[CK9] {
    import CK9._
    def map[A, B](fa: CK9[A])(f: A => B): CK9[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x))
    }
  } // FunctorCK9
