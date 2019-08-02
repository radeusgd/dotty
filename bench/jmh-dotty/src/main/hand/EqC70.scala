import Hand.{Eq, Functor}
  object EqC70 extends Eq[C70] {
    import C70._
    def eqv(x: C70, y: C70): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
      case A1(z) => y.isInstanceOf[A1] && y.asInstanceOf[A1].i == z; case B1(z) => y.isInstanceOf[B1] && y.asInstanceOf[B1].b == z; case S1(z) => y.isInstanceOf[S1] && y.asInstanceOf[S1].i == z; case D1(z) => y.isInstanceOf[D1] && y.asInstanceOf[D1].b == z; case E1(z) => y.isInstanceOf[E1] && y.asInstanceOf[E1].i == z; case F1(z) => y.isInstanceOf[F1] && y.asInstanceOf[F1].b == z; case G1(z) => y.isInstanceOf[G1] && y.asInstanceOf[G1].i == z; case H1(z) => y.isInstanceOf[H1] && y.asInstanceOf[H1].b == z; case I1(z) => y.isInstanceOf[I1] && y.asInstanceOf[I1].i == z; case J1(z) => y.isInstanceOf[J1] && y.asInstanceOf[J1].b == z
      case A2(z) => y.isInstanceOf[A2] && y.asInstanceOf[A2].i == z; case B2(z) => y.isInstanceOf[B2] && y.asInstanceOf[B2].b == z; case S2(z) => y.isInstanceOf[S2] && y.asInstanceOf[S2].i == z; case D2(z) => y.isInstanceOf[D2] && y.asInstanceOf[D2].b == z; case E2(z) => y.isInstanceOf[E2] && y.asInstanceOf[E2].i == z; case F2(z) => y.isInstanceOf[F2] && y.asInstanceOf[F2].b == z; case G2(z) => y.isInstanceOf[G2] && y.asInstanceOf[G2].i == z; case H2(z) => y.isInstanceOf[H2] && y.asInstanceOf[H2].b == z; case I2(z) => y.isInstanceOf[I2] && y.asInstanceOf[I2].i == z; case J2(z) => y.isInstanceOf[J2] && y.asInstanceOf[J2].b == z
      case A3(z) => y.isInstanceOf[A3] && y.asInstanceOf[A3].i == z; case B3(z) => y.isInstanceOf[B3] && y.asInstanceOf[B3].b == z; case S3(z) => y.isInstanceOf[S3] && y.asInstanceOf[S3].i == z; case D3(z) => y.isInstanceOf[D3] && y.asInstanceOf[D3].b == z; case E3(z) => y.isInstanceOf[E3] && y.asInstanceOf[E3].i == z; case F3(z) => y.isInstanceOf[F3] && y.asInstanceOf[F3].b == z; case G3(z) => y.isInstanceOf[G3] && y.asInstanceOf[G3].i == z; case H3(z) => y.isInstanceOf[H3] && y.asInstanceOf[H3].b == z; case I3(z) => y.isInstanceOf[I3] && y.asInstanceOf[I3].i == z; case J3(z) => y.isInstanceOf[J3] && y.asInstanceOf[J3].b == z
      case A4(z) => y.isInstanceOf[A4] && y.asInstanceOf[A4].i == z; case B4(z) => y.isInstanceOf[B4] && y.asInstanceOf[B4].b == z; case S4(z) => y.isInstanceOf[S4] && y.asInstanceOf[S4].i == z; case D4(z) => y.isInstanceOf[D4] && y.asInstanceOf[D4].b == z; case E4(z) => y.isInstanceOf[E4] && y.asInstanceOf[E4].i == z; case F4(z) => y.isInstanceOf[F4] && y.asInstanceOf[F4].b == z; case G4(z) => y.isInstanceOf[G4] && y.asInstanceOf[G4].i == z; case H4(z) => y.isInstanceOf[H4] && y.asInstanceOf[H4].b == z; case I4(z) => y.isInstanceOf[I4] && y.asInstanceOf[I4].i == z; case J4(z) => y.isInstanceOf[J4] && y.asInstanceOf[J4].b == z
      case A5(z) => y.isInstanceOf[A5] && y.asInstanceOf[A5].i == z; case B5(z) => y.isInstanceOf[B5] && y.asInstanceOf[B5].b == z; case S5(z) => y.isInstanceOf[S5] && y.asInstanceOf[S5].i == z; case D5(z) => y.isInstanceOf[D5] && y.asInstanceOf[D5].b == z; case E5(z) => y.isInstanceOf[E5] && y.asInstanceOf[E5].i == z; case F5(z) => y.isInstanceOf[F5] && y.asInstanceOf[F5].b == z; case G5(z) => y.isInstanceOf[G5] && y.asInstanceOf[G5].i == z; case H5(z) => y.isInstanceOf[H5] && y.asInstanceOf[H5].b == z; case I5(z) => y.isInstanceOf[I5] && y.asInstanceOf[I5].i == z; case J5(z) => y.isInstanceOf[J5] && y.asInstanceOf[J5].b == z
      case A6(z) => y.isInstanceOf[A6] && y.asInstanceOf[A6].i == z; case B6(z) => y.isInstanceOf[B6] && y.asInstanceOf[B6].b == z; case S6(z) => y.isInstanceOf[S6] && y.asInstanceOf[S6].i == z; case D6(z) => y.isInstanceOf[D6] && y.asInstanceOf[D6].b == z; case E6(z) => y.isInstanceOf[E6] && y.asInstanceOf[E6].i == z; case F6(z) => y.isInstanceOf[F6] && y.asInstanceOf[F6].b == z; case G6(z) => y.isInstanceOf[G6] && y.asInstanceOf[G6].i == z; case H6(z) => y.isInstanceOf[H6] && y.asInstanceOf[H6].b == z; case I6(z) => y.isInstanceOf[I6] && y.asInstanceOf[I6].i == z; case J6(z) => y.isInstanceOf[J6] && y.asInstanceOf[J6].b == z
    }
  } // EqC70
