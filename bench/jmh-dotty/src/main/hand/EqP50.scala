import Hand.{Eq, Functor}
  object EqP50 extends Eq[P50] {
    def eqv(x: P50, y: P50): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1 &&
      x.a2 == y.a2 && x.b2 == y.b2 && x.c2 == y.c2 && x.d2 == y.d2 && x.e2 == y.e2 && x.f2 == y.f2 && x.g2 == y.g2 && x.h2 == y.h2 && x.i2 == y.i2 && x.j2 == y.j2 &&
      x.a3 == y.a3 && x.b3 == y.b3 && x.c3 == y.c3 && x.d3 == y.d3 && x.e3 == y.e3 && x.f3 == y.f3 && x.g3 == y.g3 && x.h3 == y.h3 && x.i3 == y.i3 && x.j3 == y.j3 &&
      x.a4 == y.a4 && x.b4 == y.b4 && x.c4 == y.c4 && x.d4 == y.d4 && x.e4 == y.e4 && x.f4 == y.f4 && x.g4 == y.g4 && x.h4 == y.h4 && x.i4 == y.i4 && x.j4 == y.j4
    }
  } // EqP50
