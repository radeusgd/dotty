import Hand.{Eq, Functor}
  object EqP4 extends Eq[P4] {
    def eqv(x: P4, y: P4): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0
    }
  } // EqP4
