import Hand.{Eq, Functor}
  object EqP6 extends Eq[P6] {
    def eqv(x: P6, y: P6): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0
    }
  } // EqP6
