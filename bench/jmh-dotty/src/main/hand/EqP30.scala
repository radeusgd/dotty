import Hand.{Eq, Functor}
  object EqP30 extends Eq[P30] {
    def eqv(x: P30, y: P30): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1 &&
      x.a2 == y.a2 && x.b2 == y.b2 && x.c2 == y.c2 && x.d2 == y.d2 && x.e2 == y.e2 && x.f2 == y.f2 && x.g2 == y.g2 && x.h2 == y.h2 && x.i2 == y.i2 && x.j2 == y.j2
    }
  } // EqP30
