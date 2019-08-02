import Hand.{Eq, Functor}
  object EqP20 extends Eq[P20] {
    def eqv(x: P20, y: P20): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1
    }
  } // EqP20
