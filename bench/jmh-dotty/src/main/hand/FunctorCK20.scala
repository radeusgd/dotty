import Hand.{Eq, Functor}
  object FunctorCK20 extends Functor[CK20] {
    import CK20._
    def map[A, B](fa: CK20[A])(f: A => B): CK20[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
    }
  } // FunctorCK20
