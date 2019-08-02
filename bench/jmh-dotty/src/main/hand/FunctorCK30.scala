import Hand.{Eq, Functor}
  object FunctorCK30 extends Functor[CK30] {
    import CK30._
    def map[A, B](fa: CK30[A])(f: A => B): CK30[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
      case A2(x) => A2(f(x)); case B2(x) => B2(x); case S2(x) => S2(f(x)); case D2(x) => D2(x); case E2(x) => E2(f(x)); case F2(x) => F2(x); case G2(x) => G2(f(x)); case H2(x) => H2(x); case I2(x) => I2(f(x)); case J2(x) => J2(x)
    }
  } // FunctorCK30
