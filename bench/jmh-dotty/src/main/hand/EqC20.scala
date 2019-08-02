import Hand.{Eq, Functor}
  object EqC20 extends Eq[C20] {
    import C20._
    def eqv(x: C20, y: C20): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
      case A1(z) => y.isInstanceOf[A1] && y.asInstanceOf[A1].i == z; case B1(z) => y.isInstanceOf[B1] && y.asInstanceOf[B1].b == z; case S1(z) => y.isInstanceOf[S1] && y.asInstanceOf[S1].i == z; case D1(z) => y.isInstanceOf[D1] && y.asInstanceOf[D1].b == z; case E1(z) => y.isInstanceOf[E1] && y.asInstanceOf[E1].i == z; case F1(z) => y.isInstanceOf[F1] && y.asInstanceOf[F1].b == z; case G1(z) => y.isInstanceOf[G1] && y.asInstanceOf[G1].i == z; case H1(z) => y.isInstanceOf[H1] && y.asInstanceOf[H1].b == z; case I1(z) => y.isInstanceOf[I1] && y.asInstanceOf[I1].i == z; case J1(z) => y.isInstanceOf[J1] && y.asInstanceOf[J1].b == z
    }
  } // EqC20
