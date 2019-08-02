import Hand.{Eq, Functor}
  object EqP9 extends Eq[P9] {
    def eqv(x: P9, y: P9): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0
    }
  } // EqP9
