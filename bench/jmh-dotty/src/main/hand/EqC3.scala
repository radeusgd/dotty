import Hand.{Eq, Functor}
  object EqC3 extends Eq[C3] {
    import C3._
    def eqv(x: C3, y: C3): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z
    }
  } // EqC3
