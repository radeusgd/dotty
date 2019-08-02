import Hand.{Eq, Functor}
  object EqP3 extends Eq[P3] {
    def eqv(x: P3, y: P3): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0
    }
  } // EqP3
