import Hand.{Eq, Functor}
  object EqP1 extends Eq[P1] {
    def eqv(x: P1, y: P1): Boolean = {
      x.a0 == y.a0
    }
  } // EqP1
