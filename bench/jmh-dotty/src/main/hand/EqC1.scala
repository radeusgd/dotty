import Hand.{Eq, Functor}
  object EqC1 extends Eq[C1] {
    import C1._
    def eqv(x: C1, y: C1): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z
    }
  } // EqC1
