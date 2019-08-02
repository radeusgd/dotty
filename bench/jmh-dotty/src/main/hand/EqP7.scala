import Hand.{Eq, Functor}
  object EqP7 extends Eq[P7] {
    def eqv(x: P7, y: P7): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0
    }
  } // EqP7
