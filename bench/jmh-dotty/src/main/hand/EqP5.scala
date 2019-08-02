import Hand.{Eq, Functor}
  object EqP5 extends Eq[P5] {
    def eqv(x: P5, y: P5): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0
    }
  } // EqP5
