import Hand.{Eq, Functor}
  object EqC8 extends Eq[C8] {
    import C8._
    def eqv(x: C8, y: C8): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z
    }
  } // EqC8
