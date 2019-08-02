import Hand.{Eq, Functor}
  object FunctorCK50 extends Functor[CK50] {
    import CK50._
    def map[A, B](fa: CK50[A])(f: A => B): CK50[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
      case A2(x) => A2(f(x)); case B2(x) => B2(x); case S2(x) => S2(f(x)); case D2(x) => D2(x); case E2(x) => E2(f(x)); case F2(x) => F2(x); case G2(x) => G2(f(x)); case H2(x) => H2(x); case I2(x) => I2(f(x)); case J2(x) => J2(x)
      case A3(x) => A3(f(x)); case B3(x) => B3(x); case S3(x) => S3(f(x)); case D3(x) => D3(x); case E3(x) => E3(f(x)); case F3(x) => F3(x); case G3(x) => G3(f(x)); case H3(x) => H3(x); case I3(x) => I3(f(x)); case J3(x) => J3(x)
      case A4(x) => A4(f(x)); case B4(x) => B4(x); case S4(x) => S4(f(x)); case D4(x) => D4(x); case E4(x) => E4(f(x)); case F4(x) => F4(x); case G4(x) => G4(f(x)); case H4(x) => H4(x); case I4(x) => I4(f(x)); case J4(x) => J4(x)
    }
  } // FunctorCK50
