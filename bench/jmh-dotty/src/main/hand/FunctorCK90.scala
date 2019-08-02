import Hand.{Eq, Functor}
  object FunctorCK90 extends Functor[CK90] {
    import CK90._
    def map[A, B](fa: CK90[A])(f: A => B): CK90[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
      case A2(x) => A2(f(x)); case B2(x) => B2(x); case S2(x) => S2(f(x)); case D2(x) => D2(x); case E2(x) => E2(f(x)); case F2(x) => F2(x); case G2(x) => G2(f(x)); case H2(x) => H2(x); case I2(x) => I2(f(x)); case J2(x) => J2(x)
      case A3(x) => A3(f(x)); case B3(x) => B3(x); case S3(x) => S3(f(x)); case D3(x) => D3(x); case E3(x) => E3(f(x)); case F3(x) => F3(x); case G3(x) => G3(f(x)); case H3(x) => H3(x); case I3(x) => I3(f(x)); case J3(x) => J3(x)
      case A4(x) => A4(f(x)); case B4(x) => B4(x); case S4(x) => S4(f(x)); case D4(x) => D4(x); case E4(x) => E4(f(x)); case F4(x) => F4(x); case G4(x) => G4(f(x)); case H4(x) => H4(x); case I4(x) => I4(f(x)); case J4(x) => J4(x)
      case A5(x) => A5(f(x)); case B5(x) => B5(x); case S5(x) => S5(f(x)); case D5(x) => D5(x); case E5(x) => E5(f(x)); case F5(x) => F5(x); case G5(x) => G5(f(x)); case H5(x) => H5(x); case I5(x) => I5(f(x)); case J5(x) => J5(x)
      case A6(x) => A6(f(x)); case B6(x) => B6(x); case S6(x) => S6(f(x)); case D6(x) => D6(x); case E6(x) => E6(f(x)); case F6(x) => F6(x); case G6(x) => G6(f(x)); case H6(x) => H6(x); case I6(x) => I6(f(x)); case J6(x) => J6(x)
      case A7(x) => A7(f(x)); case B7(x) => B7(x); case S7(x) => S7(f(x)); case D7(x) => D7(x); case E7(x) => E7(f(x)); case F7(x) => F7(x); case G7(x) => G7(f(x)); case H7(x) => H7(x); case I7(x) => I7(f(x)); case J7(x) => J7(x)
      case A8(x) => A8(f(x)); case B8(x) => B8(x); case S8(x) => S8(f(x)); case D8(x) => D8(x); case E8(x) => E8(f(x)); case F8(x) => F8(x); case G8(x) => G8(f(x)); case H8(x) => H8(x); case I8(x) => I8(f(x)); case J8(x) => J8(x)
    }
  } // FunctorCK90
