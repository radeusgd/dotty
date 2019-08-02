import Hand.{Eq, Functor}
  object EqP2 extends Eq[P2] {
    def eqv(x: P2, y: P2): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0
    }
  } // EqP2
