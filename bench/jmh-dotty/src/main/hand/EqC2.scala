import Hand.{Eq, Functor}
  object EqC2 extends Eq[C2] {
    import C2._
    def eqv(x: C2, y: C2): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z
    }
  } // EqC2
