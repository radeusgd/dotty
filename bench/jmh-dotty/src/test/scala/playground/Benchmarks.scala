package playground

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class Benchmarks {
  val p0_a: P0 = P0()
  val p0_b: P0 = P0()
  val p1_a: P1 = P1(0)
  val p1_b: P1 = P1(0)
  val p2_a: P2 = P2(0, true)
  val p2_b: P2 = P2(0, true)
  val p3_a: P3 = P3(0, true, 0)
  val p3_b: P3 = P3(0, true, 0)
  val p4_a: P4 = P4(0, true, 0, true)
  val p4_b: P4 = P4(0, true, 0, true)
  val p5_a: P5 = P5(0, true, 0, true, 0)
  val p5_b: P5 = P5(0, true, 0, true, 0)
  val p6_a: P6 = P6(0, true, 0, true, 0, true)
  val p6_b: P6 = P6(0, true, 0, true, 0, true)
  val p7_a: P7 = P7(0, true, 0, true, 0, true, 0)
  val p7_b: P7 = P7(0, true, 0, true, 0, true, 0)
  val p8_a: P8 = P8(0, true, 0, true, 0, true, 0, true)
  val p8_b: P8 = P8(0, true, 0, true, 0, true, 0, true)
  val p9_a: P9 = P9(0, true, 0, true, 0, true, 0, true, 0)
  val p9_b: P9 = P9(0, true, 0, true, 0, true, 0, true, 0)
  val p10_a: P10 = P10(0, true, 0, true, 0, true, 0, true, 0, true)
  val p10_b: P10 = P10(0, true, 0, true, 0, true, 0, true, 0, true)
  val p20_a: P20 = P20(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p20_b: P20 = P20(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p30_a: P30 = P30(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p30_b: P30 = P30(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p40_a: P40 = P40(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p40_b: P40 = P40(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p50_a: P50 = P50(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p50_b: P50 = P50(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p60_a: P60 = P60(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p60_b: P60 = P60(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p70_a: P70 = P70(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p70_b: P70 = P70(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p80_a: P80 = P80(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p80_b: P80 = P80(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p90_a: P90 = P90(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p90_b: P90 = P90(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p100_a: P100 = P100(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)
  val p100_b: P100 = P100(0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true, 0, true)

  val inlinedEqDerivedC1  = Inlined.Eq.derived[C1]
  val inlinedEqDerivedC2  = Inlined.Eq.derived[C2]
  val inlinedEqDerivedC3  = Inlined.Eq.derived[C3]
  val inlinedEqDerivedC4  = Inlined.Eq.derived[C4]
  val inlinedEqDerivedC5  = Inlined.Eq.derived[C5]
  val inlinedEqDerivedC6  = Inlined.Eq.derived[C6]
  val inlinedEqDerivedC7  = Inlined.Eq.derived[C7]
  val inlinedEqDerivedC8  = Inlined.Eq.derived[C8]
  val inlinedEqDerivedC9  = Inlined.Eq.derived[C9]
  val inlinedEqDerivedC10  = Inlined.Eq.derived[C10]
  val inlinedEqDerivedC20  = Inlined.Eq.derived[C20]
  val inlinedEqDerivedC30  = Inlined.Eq.derived[C30]
  val inlinedEqDerivedC40  = Inlined.Eq.derived[C40]
  val inlinedEqDerivedC50  = Inlined.Eq.derived[C50]
  val inlinedEqDerivedC60  = Inlined.Eq.derived[C60]
  val inlinedEqDerivedC70  = Inlined.Eq.derived[C70]
  val inlinedEqDerivedC80  = Inlined.Eq.derived[C80]
  val inlinedEqDerivedC90  = Inlined.Eq.derived[C90]
  val inlinedEqDerivedC100 = Inlined.Eq.derived[C100]

  val inlinedEqDerivedP0   = Inlined.Eq.derived[P0]
  val inlinedEqDerivedP1   = Inlined.Eq.derived[P1]
  val inlinedEqDerivedP2   = Inlined.Eq.derived[P2]
  val inlinedEqDerivedP3   = Inlined.Eq.derived[P3]
  val inlinedEqDerivedP4   = Inlined.Eq.derived[P4]
  val inlinedEqDerivedP5   = Inlined.Eq.derived[P5]
  val inlinedEqDerivedP6   = Inlined.Eq.derived[P6]
  val inlinedEqDerivedP7   = Inlined.Eq.derived[P7]
  val inlinedEqDerivedP8   = Inlined.Eq.derived[P8]
  val inlinedEqDerivedP9   = Inlined.Eq.derived[P9]
  val inlinedEqDerivedP10  = Inlined.Eq.derived[P10]
  val inlinedEqDerivedP20  = Inlined.Eq.derived[P20]
  val inlinedEqDerivedP30  = Inlined.Eq.derived[P30]
  val inlinedEqDerivedP40  = Inlined.Eq.derived[P40]
  val inlinedEqDerivedP50  = Inlined.Eq.derived[P50]
  val inlinedEqDerivedP60  = Inlined.Eq.derived[P60]
  val inlinedEqDerivedP70  = Inlined.Eq.derived[P70]
  val inlinedEqDerivedP80  = Inlined.Eq.derived[P80]
  val inlinedEqDerivedP90  = Inlined.Eq.derived[P90]
  val inlinedEqDerivedP100 = Inlined.Eq.derived[P100]

  val stagedEqDerivedC1   = Staged.Eq.derived[C1]
  val stagedEqDerivedC2   = Staged.Eq.derived[C2]
  val stagedEqDerivedC3   = Staged.Eq.derived[C3]
  val stagedEqDerivedC4   = Staged.Eq.derived[C4]
  val stagedEqDerivedC5   = Staged.Eq.derived[C5]
  val stagedEqDerivedC6   = Staged.Eq.derived[C6]
  val stagedEqDerivedC7   = Staged.Eq.derived[C7]
  val stagedEqDerivedC8   = Staged.Eq.derived[C8]
  val stagedEqDerivedC9   = Staged.Eq.derived[C9]
  val stagedEqDerivedC10   = Staged.Eq.derived[C10]
  val stagedEqDerivedC20   = Staged.Eq.derived[C20]
  val stagedEqDerivedC30   = Staged.Eq.derived[C30]
  val stagedEqDerivedC40   = Staged.Eq.derived[C40]
  val stagedEqDerivedC50   = Staged.Eq.derived[C50]
  val stagedEqDerivedC60   = Staged.Eq.derived[C60]
  val stagedEqDerivedC70   = Staged.Eq.derived[C70]
  val stagedEqDerivedC80   = Staged.Eq.derived[C80]
  val stagedEqDerivedC90   = Staged.Eq.derived[C90]
  val stagedEqDerivedC100  = Staged.Eq.derived[C100]

  val stagedEqDerivedP0    = Staged.Eq.derived[P0]
  val stagedEqDerivedP1   = Staged.Eq.derived[P1]
  val stagedEqDerivedP2   = Staged.Eq.derived[P2]
  val stagedEqDerivedP3   = Staged.Eq.derived[P3]
  val stagedEqDerivedP4   = Staged.Eq.derived[P4]
  val stagedEqDerivedP5   = Staged.Eq.derived[P5]
  val stagedEqDerivedP6   = Staged.Eq.derived[P6]
  val stagedEqDerivedP7   = Staged.Eq.derived[P7]
  val stagedEqDerivedP8   = Staged.Eq.derived[P8]
  val stagedEqDerivedP9   = Staged.Eq.derived[P9]
  val stagedEqDerivedP10   = Staged.Eq.derived[P10]
  val stagedEqDerivedP20   = Staged.Eq.derived[P20]
  val stagedEqDerivedP30   = Staged.Eq.derived[P30]
  val stagedEqDerivedP40   = Staged.Eq.derived[P40]
  val stagedEqDerivedP50   = Staged.Eq.derived[P50]
  val stagedEqDerivedP60   = Staged.Eq.derived[P60]
  val stagedEqDerivedP70   = Staged.Eq.derived[P70]
  val stagedEqDerivedP80   = Staged.Eq.derived[P80]
  val stagedEqDerivedP90   = Staged.Eq.derived[P90]
  val stagedEqDerivedP100  = Staged.Eq.derived[P100]

  import given Shapeless3.K0._

  val shapeless3EqDerivedC1   = Shapeless3.Eq.derived[C1]
  val shapeless3EqDerivedC2   = Shapeless3.Eq.derived[C2]
  val shapeless3EqDerivedC3   = Shapeless3.Eq.derived[C3]
  val shapeless3EqDerivedC4   = Shapeless3.Eq.derived[C4]
  val shapeless3EqDerivedC5   = Shapeless3.Eq.derived[C5]
  val shapeless3EqDerivedC6   = Shapeless3.Eq.derived[C6]
  val shapeless3EqDerivedC7   = Shapeless3.Eq.derived[C7]
  val shapeless3EqDerivedC8   = Shapeless3.Eq.derived[C8]
  val shapeless3EqDerivedC9   = Shapeless3.Eq.derived[C9]
  val shapeless3EqDerivedC10   = Shapeless3.Eq.derived[C10]
  val shapeless3EqDerivedC20   = Shapeless3.Eq.derived[C20]
  val shapeless3EqDerivedC30   = Shapeless3.Eq.derived[C30]
  val shapeless3EqDerivedC40   = Shapeless3.Eq.derived[C40]
  val shapeless3EqDerivedC50   = Shapeless3.Eq.derived[C50]
  val shapeless3EqDerivedC60   = Shapeless3.Eq.derived[C60]
  val shapeless3EqDerivedC70   = Shapeless3.Eq.derived[C70]
  val shapeless3EqDerivedC80   = Shapeless3.Eq.derived[C80]
  val shapeless3EqDerivedC90   = Shapeless3.Eq.derived[C90]
  val shapeless3EqDerivedC100  = Shapeless3.Eq.derived[C100]

  val shapeless3EqDerivedP0    = Shapeless3.Eq.derived[P0]
  val shapeless3EqDerivedP1   = Shapeless3.Eq.derived[P1]
  val shapeless3EqDerivedP2   = Shapeless3.Eq.derived[P2]
  val shapeless3EqDerivedP3   = Shapeless3.Eq.derived[P3]
  val shapeless3EqDerivedP4   = Shapeless3.Eq.derived[P4]
  val shapeless3EqDerivedP5   = Shapeless3.Eq.derived[P5]
  val shapeless3EqDerivedP6   = Shapeless3.Eq.derived[P6]
  val shapeless3EqDerivedP7   = Shapeless3.Eq.derived[P7]
  val shapeless3EqDerivedP8   = Shapeless3.Eq.derived[P8]
  val shapeless3EqDerivedP9   = Shapeless3.Eq.derived[P9]
  val shapeless3EqDerivedP10   = Shapeless3.Eq.derived[P10]
  val shapeless3EqDerivedP20   = Shapeless3.Eq.derived[P20]
  val shapeless3EqDerivedP30   = Shapeless3.Eq.derived[P30]
  val shapeless3EqDerivedP40   = Shapeless3.Eq.derived[P40]
  val shapeless3EqDerivedP50   = Shapeless3.Eq.derived[P50]
  val shapeless3EqDerivedP60   = Shapeless3.Eq.derived[P60]
  val shapeless3EqDerivedP70   = Shapeless3.Eq.derived[P70]
  val shapeless3EqDerivedP80   = Shapeless3.Eq.derived[P80]
  val shapeless3EqDerivedP90   = Shapeless3.Eq.derived[P90]
  val shapeless3EqDerivedP100  = Shapeless3.Eq.derived[P100]

  val stagedFunctorDerivedCK1   = Staged.Functor.derived[CK1]
  val stagedFunctorDerivedCK2   = Staged.Functor.derived[CK2]
  val stagedFunctorDerivedCK3   = Staged.Functor.derived[CK3]
  val stagedFunctorDerivedCK4   = Staged.Functor.derived[CK4]
  val stagedFunctorDerivedCK5   = Staged.Functor.derived[CK5]
  val stagedFunctorDerivedCK6   = Staged.Functor.derived[CK6]
  val stagedFunctorDerivedCK7   = Staged.Functor.derived[CK7]
  val stagedFunctorDerivedCK8   = Staged.Functor.derived[CK8]
  val stagedFunctorDerivedCK9   = Staged.Functor.derived[CK9]
  val stagedFunctorDerivedCK10   = Staged.Functor.derived[CK10]
  val stagedFunctorDerivedCK20   = Staged.Functor.derived[CK20]
  val stagedFunctorDerivedCK30   = Staged.Functor.derived[CK30]
  val stagedFunctorDerivedCK40   = Staged.Functor.derived[CK40]
  val stagedFunctorDerivedCK50   = Staged.Functor.derived[CK50]
  val stagedFunctorDerivedCK60   = Staged.Functor.derived[CK60]
  val stagedFunctorDerivedCK70   = Staged.Functor.derived[CK70]
  val stagedFunctorDerivedCK80   = Staged.Functor.derived[CK80]
  val stagedFunctorDerivedCK90   = Staged.Functor.derived[CK90]
  val stagedFunctorDerivedCK100  = Staged.Functor.derived[CK100]

  val stagedFunctorDerivedPK0    = Staged.Functor.derived[PK0]
  val stagedFunctorDerivedPK1   = Staged.Functor.derived[PK1]
  val stagedFunctorDerivedPK2   = Staged.Functor.derived[PK2]
  val stagedFunctorDerivedPK3   = Staged.Functor.derived[PK3]
  val stagedFunctorDerivedPK4   = Staged.Functor.derived[PK4]
  val stagedFunctorDerivedPK5   = Staged.Functor.derived[PK5]
  val stagedFunctorDerivedPK6   = Staged.Functor.derived[PK6]
  val stagedFunctorDerivedPK7   = Staged.Functor.derived[PK7]
  val stagedFunctorDerivedPK8   = Staged.Functor.derived[PK8]
  val stagedFunctorDerivedPK9   = Staged.Functor.derived[PK9]
  val stagedFunctorDerivedPK10   = Staged.Functor.derived[PK10]
  val stagedFunctorDerivedPK20   = Staged.Functor.derived[PK20]
  val stagedFunctorDerivedPK30   = Staged.Functor.derived[PK30]
  val stagedFunctorDerivedPK40   = Staged.Functor.derived[PK40]
  val stagedFunctorDerivedPK50   = Staged.Functor.derived[PK50]
  val stagedFunctorDerivedPK60   = Staged.Functor.derived[PK60]
  val stagedFunctorDerivedPK70   = Staged.Functor.derived[PK70]
  val stagedFunctorDerivedPK80   = Staged.Functor.derived[PK80]
  val stagedFunctorDerivedPK90   = Staged.Functor.derived[PK90]
  val stagedFunctorDerivedPK100  = Staged.Functor.derived[PK100]

  import given Shapeless3.K1._

  val shapeless3FunctorDerivedCK1   = Shapeless3.Functor.derived[CK1]
  val shapeless3FunctorDerivedCK2   = Shapeless3.Functor.derived[CK2]
  val shapeless3FunctorDerivedCK3   = Shapeless3.Functor.derived[CK3]
  val shapeless3FunctorDerivedCK4   = Shapeless3.Functor.derived[CK4]
  val shapeless3FunctorDerivedCK5   = Shapeless3.Functor.derived[CK5]
  val shapeless3FunctorDerivedCK6   = Shapeless3.Functor.derived[CK6]
  val shapeless3FunctorDerivedCK7   = Shapeless3.Functor.derived[CK7]
  val shapeless3FunctorDerivedCK8   = Shapeless3.Functor.derived[CK8]
  val shapeless3FunctorDerivedCK9   = Shapeless3.Functor.derived[CK9]
  val shapeless3FunctorDerivedCK10   = Shapeless3.Functor.derived[CK10]
  val shapeless3FunctorDerivedCK20   = Shapeless3.Functor.derived[CK20]
  val shapeless3FunctorDerivedCK30   = Shapeless3.Functor.derived[CK30]
  val shapeless3FunctorDerivedCK40   = Shapeless3.Functor.derived[CK40]
  val shapeless3FunctorDerivedCK50   = Shapeless3.Functor.derived[CK50]
  val shapeless3FunctorDerivedCK60   = Shapeless3.Functor.derived[CK60]
  val shapeless3FunctorDerivedCK70   = Shapeless3.Functor.derived[CK70]
  val shapeless3FunctorDerivedCK80   = Shapeless3.Functor.derived[CK80]
  val shapeless3FunctorDerivedCK90   = Shapeless3.Functor.derived[CK90]
  val shapeless3FunctorDerivedCK100  = Shapeless3.Functor.derived[CK100]

  val shapeless3FunctorDerivedPK0    = Shapeless3.Functor.derived[PK0]
  val shapeless3FunctorDerivedPK1   = Shapeless3.Functor.derived[PK1]
  val shapeless3FunctorDerivedPK2   = Shapeless3.Functor.derived[PK2]
  val shapeless3FunctorDerivedPK3   = Shapeless3.Functor.derived[PK3]
  val shapeless3FunctorDerivedPK4   = Shapeless3.Functor.derived[PK4]
  val shapeless3FunctorDerivedPK5   = Shapeless3.Functor.derived[PK5]
  val shapeless3FunctorDerivedPK6   = Shapeless3.Functor.derived[PK6]
  val shapeless3FunctorDerivedPK7   = Shapeless3.Functor.derived[PK7]
  val shapeless3FunctorDerivedPK8   = Shapeless3.Functor.derived[PK8]
  val shapeless3FunctorDerivedPK9   = Shapeless3.Functor.derived[PK9]
  val shapeless3FunctorDerivedPK10   = Shapeless3.Functor.derived[PK10]
  val shapeless3FunctorDerivedPK20   = Shapeless3.Functor.derived[PK20]
  val shapeless3FunctorDerivedPK30   = Shapeless3.Functor.derived[PK30]
  val shapeless3FunctorDerivedPK40   = Shapeless3.Functor.derived[PK40]
  val shapeless3FunctorDerivedPK50   = Shapeless3.Functor.derived[PK50]
  val shapeless3FunctorDerivedPK60   = Shapeless3.Functor.derived[PK60]
  val shapeless3FunctorDerivedPK70   = Shapeless3.Functor.derived[PK70]
  val shapeless3FunctorDerivedPK80   = Shapeless3.Functor.derived[PK80]
  val shapeless3FunctorDerivedPK90   = Shapeless3.Functor.derived[PK90]
  val shapeless3FunctorDerivedPK100  = Shapeless3.Functor.derived[PK100]

  @Benchmark def assertInlinedEqDerivedP00EqvP0AP0B: Unit =
                 assert(inlinedEqDerivedP0.eqv(p0_a, p0_b))
  @Benchmark def assertInlinedEqDerivedP01EqvP1AP1B: Unit =
                 assert(inlinedEqDerivedP1.eqv(p1_a, p1_b))
  @Benchmark def assertInlinedEqDerivedP02EqvP2AP2B: Unit =
                 assert(inlinedEqDerivedP2.eqv(p2_a, p2_b))
  @Benchmark def assertInlinedEqDerivedP03EqvP3AP3B: Unit =
                 assert(inlinedEqDerivedP3.eqv(p3_a, p3_b))
  @Benchmark def assertInlinedEqDerivedP04EqvP4AP4B: Unit =
                 assert(inlinedEqDerivedP4.eqv(p4_a, p4_b))
  @Benchmark def assertInlinedEqDerivedP05EqvP5AP5B: Unit =
                 assert(inlinedEqDerivedP5.eqv(p5_a, p5_b))
  @Benchmark def assertInlinedEqDerivedP06EqvP6AP6B: Unit =
                 assert(inlinedEqDerivedP6.eqv(p6_a, p6_b))
  @Benchmark def assertInlinedEqDerivedP07EqvP7AP7B: Unit =
                 assert(inlinedEqDerivedP7.eqv(p7_a, p7_b))
  @Benchmark def assertInlinedEqDerivedP08EqvP8AP8B: Unit =
                 assert(inlinedEqDerivedP8.eqv(p8_a, p8_b))
  @Benchmark def assertInlinedEqDerivedP09EqvP9AP9B: Unit =
                 assert(inlinedEqDerivedP9.eqv(p9_a, p9_b))
  @Benchmark def assertInlinedEqDerivedP10EqvP10AP10B: Unit =
                 assert(inlinedEqDerivedP10.eqv(p10_a, p10_b))
  @Benchmark def assertInlinedEqDerivedP20EqvP20AP20B: Unit =
                 assert(inlinedEqDerivedP20.eqv(p20_a, p20_b))
  @Benchmark def assertInlinedEqDerivedP30EqvP30AP30B: Unit =
                 assert(inlinedEqDerivedP30.eqv(p30_a, p30_b))
  @Benchmark def assertInlinedEqDerivedP40EqvP40AP40B: Unit =
                 assert(inlinedEqDerivedP40.eqv(p40_a, p40_b))
  @Benchmark def assertInlinedEqDerivedP50EqvP50AP50B: Unit =
                 assert(inlinedEqDerivedP50.eqv(p50_a, p50_b))
  @Benchmark def assertInlinedEqDerivedP60EqvP60AP60B: Unit =
                 assert(inlinedEqDerivedP60.eqv(p60_a, p60_b))
  @Benchmark def assertInlinedEqDerivedP70EqvP70AP70B: Unit =
                 assert(inlinedEqDerivedP70.eqv(p70_a, p70_b))
  @Benchmark def assertInlinedEqDerivedP80EqvP80AP80B: Unit =
                 assert(inlinedEqDerivedP80.eqv(p80_a, p80_b))
  @Benchmark def assertInlinedEqDerivedP90EqvP90AP90B: Unit =
                 assert(inlinedEqDerivedP90.eqv(p90_a, p90_b))
  @Benchmark def assertInlinedEqDerivedP99EqvP99AP99B: Unit =
                 assert(inlinedEqDerivedP100.eqv(p100_a, p100_b))

  @Benchmark def assertStagedEqDerivedP00EqvP0AP0B: Unit =
                 assert(stagedEqDerivedP0.eqv(p0_a, p0_b))
  @Benchmark def assertStagedEqDerivedP01EqvP1AP1B: Unit =
                 assert(stagedEqDerivedP1.eqv(p1_a, p1_b))
  @Benchmark def assertStagedEqDerivedP02EqvP2AP2B: Unit =
                 assert(stagedEqDerivedP2.eqv(p2_a, p2_b))
  @Benchmark def assertStagedEqDerivedP03EqvP3AP3B: Unit =
                 assert(stagedEqDerivedP3.eqv(p3_a, p3_b))
  @Benchmark def assertStagedEqDerivedP04EqvP4AP4B: Unit =
                 assert(stagedEqDerivedP4.eqv(p4_a, p4_b))
  @Benchmark def assertStagedEqDerivedP05EqvP5AP5B: Unit =
                 assert(stagedEqDerivedP5.eqv(p5_a, p5_b))
  @Benchmark def assertStagedEqDerivedP06EqvP6AP6B: Unit =
                 assert(stagedEqDerivedP6.eqv(p6_a, p6_b))
  @Benchmark def assertStagedEqDerivedP07EqvP7AP7B: Unit =
                 assert(stagedEqDerivedP7.eqv(p7_a, p7_b))
  @Benchmark def assertStagedEqDerivedP08EqvP8AP8B: Unit =
                 assert(stagedEqDerivedP8.eqv(p8_a, p8_b))
  @Benchmark def assertStagedEqDerivedP09EqvP9AP9B: Unit =
                 assert(stagedEqDerivedP9.eqv(p9_a, p9_b))
  @Benchmark def assertStagedEqDerivedP10EqvP10AP10B: Unit =
                 assert(stagedEqDerivedP10.eqv(p10_a, p10_b))
  @Benchmark def assertStagedEqDerivedP20EqvP20AP20B: Unit =
                 assert(stagedEqDerivedP20.eqv(p20_a, p20_b))
  @Benchmark def assertStagedEqDerivedP30EqvP30AP30B: Unit =
                 assert(stagedEqDerivedP30.eqv(p30_a, p30_b))
  @Benchmark def assertStagedEqDerivedP40EqvP40AP40B: Unit =
                 assert(stagedEqDerivedP40.eqv(p40_a, p40_b))
  @Benchmark def assertStagedEqDerivedP50EqvP50AP50B: Unit =
                 assert(stagedEqDerivedP50.eqv(p50_a, p50_b))
  @Benchmark def assertStagedEqDerivedP60EqvP60AP60B: Unit =
                 assert(stagedEqDerivedP60.eqv(p60_a, p60_b))
  @Benchmark def assertStagedEqDerivedP70EqvP70AP70B: Unit =
                 assert(stagedEqDerivedP70.eqv(p70_a, p70_b))
  @Benchmark def assertStagedEqDerivedP80EqvP80AP80B: Unit =
                 assert(stagedEqDerivedP80.eqv(p80_a, p80_b))
  @Benchmark def assertStagedEqDerivedP90EqvP90AP90B: Unit =
                 assert(stagedEqDerivedP90.eqv(p90_a, p90_b))
  @Benchmark def assertStagedEqDerivedP99EqvP99AP99B: Unit =
                 assert(stagedEqDerivedP100.eqv(p100_a, p100_b))

  @Benchmark def assertShapeless3EqDerivedP00EqvP0AP0B: Unit =
                 assert(shapeless3EqDerivedP0.eqv(p0_a, p0_b))
  @Benchmark def assertShapeless3EqDerivedP01EqvP1AP1B: Unit =
                 assert(shapeless3EqDerivedP1.eqv(p1_a, p1_b))
  @Benchmark def assertShapeless3EqDerivedP02EqvP2AP2B: Unit =
                 assert(shapeless3EqDerivedP2.eqv(p2_a, p2_b))
  @Benchmark def assertShapeless3EqDerivedP03EqvP3AP3B: Unit =
                 assert(shapeless3EqDerivedP3.eqv(p3_a, p3_b))
  @Benchmark def assertShapeless3EqDerivedP04EqvP4AP4B: Unit =
                 assert(shapeless3EqDerivedP4.eqv(p4_a, p4_b))
  @Benchmark def assertShapeless3EqDerivedP05EqvP5AP5B: Unit =
                 assert(shapeless3EqDerivedP5.eqv(p5_a, p5_b))
  @Benchmark def assertShapeless3EqDerivedP06EqvP6AP6B: Unit =
                 assert(shapeless3EqDerivedP6.eqv(p6_a, p6_b))
  @Benchmark def assertShapeless3EqDerivedP07EqvP7AP7B: Unit =
                 assert(shapeless3EqDerivedP7.eqv(p7_a, p7_b))
  @Benchmark def assertShapeless3EqDerivedP08EqvP8AP8B: Unit =
                 assert(shapeless3EqDerivedP8.eqv(p8_a, p8_b))
  @Benchmark def assertShapeless3EqDerivedP09EqvP9AP9B: Unit =
                 assert(shapeless3EqDerivedP9.eqv(p9_a, p9_b))
  @Benchmark def assertShapeless3EqDerivedP10EqvP10AP10B: Unit =
                 assert(shapeless3EqDerivedP10.eqv(p10_a, p10_b))
  @Benchmark def assertShapeless3EqDerivedP20EqvP20AP20B: Unit =
                 assert(shapeless3EqDerivedP20.eqv(p20_a, p20_b))
  @Benchmark def assertShapeless3EqDerivedP30EqvP30AP30B: Unit =
                 assert(shapeless3EqDerivedP30.eqv(p30_a, p30_b))
  @Benchmark def assertShapeless3EqDerivedP40EqvP40AP40B: Unit =
                 assert(shapeless3EqDerivedP40.eqv(p40_a, p40_b))
  @Benchmark def assertShapeless3EqDerivedP50EqvP50AP50B: Unit =
                 assert(shapeless3EqDerivedP50.eqv(p50_a, p50_b))
  @Benchmark def assertShapeless3EqDerivedP60EqvP60AP60B: Unit =
                 assert(shapeless3EqDerivedP60.eqv(p60_a, p60_b))
  @Benchmark def assertShapeless3EqDerivedP70EqvP70AP70B: Unit =
                 assert(shapeless3EqDerivedP70.eqv(p70_a, p70_b))
  @Benchmark def assertShapeless3EqDerivedP80EqvP80AP80B: Unit =
                 assert(shapeless3EqDerivedP80.eqv(p80_a, p80_b))
  @Benchmark def assertShapeless3EqDerivedP90EqvP90AP90B: Unit =
                 assert(shapeless3EqDerivedP90.eqv(p90_a, p90_b))
  @Benchmark def assertShapeless3EqDerivedP99EqvP99AP99B: Unit =
                 assert(shapeless3EqDerivedP100.eqv(p100_a, p100_b))

  @Benchmark def assertHandEqP00EqvP0AP0B: Unit =
                 assert(Hand.EqP0.eqv(p0_a, p0_b))
  @Benchmark def assertHandEqP01EqvP1AP1B: Unit =
                 assert(Hand.EqP1.eqv(p1_a, p1_b))
  @Benchmark def assertHandEqP02EqvP2AP2B: Unit =
                 assert(Hand.EqP2.eqv(p2_a, p2_b))
  @Benchmark def assertHandEqP03EqvP3AP3B: Unit =
                 assert(Hand.EqP3.eqv(p3_a, p3_b))
  @Benchmark def assertHandEqP04EqvP4AP4B: Unit =
                 assert(Hand.EqP4.eqv(p4_a, p4_b))
  @Benchmark def assertHandEqP05EqvP5AP5B: Unit =
                 assert(Hand.EqP5.eqv(p5_a, p5_b))
  @Benchmark def assertHandEqP06EqvP6AP6B: Unit =
                 assert(Hand.EqP6.eqv(p6_a, p6_b))
  @Benchmark def assertHandEqP07EqvP7AP7B: Unit =
                 assert(Hand.EqP7.eqv(p7_a, p7_b))
  @Benchmark def assertHandEqP08EqvP8AP8B: Unit =
                 assert(Hand.EqP8.eqv(p8_a, p8_b))
  @Benchmark def assertHandEqP09EqvP9AP9B: Unit =
                 assert(Hand.EqP9.eqv(p9_a, p9_b))
  @Benchmark def assertHandEqP10EqvP10AP10B: Unit =
                 assert(Hand.EqP10.eqv(p10_a, p10_b))
  @Benchmark def assertHandEqP20EqvP20AP20B: Unit =
                 assert(Hand.EqP20.eqv(p20_a, p20_b))
  @Benchmark def assertHandEqP30EqvP30AP30B: Unit =
                 assert(Hand.EqP30.eqv(p30_a, p30_b))
  @Benchmark def assertHandEqP40EqvP40AP40B: Unit =
                 assert(Hand.EqP40.eqv(p40_a, p40_b))
  @Benchmark def assertHandEqP50EqvP50AP50B: Unit =
                 assert(Hand.EqP50.eqv(p50_a, p50_b))
  @Benchmark def assertHandEqP60EqvP60AP60B: Unit =
                 assert(Hand.EqP60.eqv(p60_a, p60_b))
  @Benchmark def assertHandEqP70EqvP70AP70B: Unit =
                 assert(Hand.EqP70.eqv(p70_a, p70_b))
  @Benchmark def assertHandEqP80EqvP80AP80B: Unit =
                 assert(Hand.EqP80.eqv(p80_a, p80_b))
  @Benchmark def assertHandEqP90EqvP90AP90B: Unit =
                 assert(Hand.EqP90.eqv(p90_a, p90_b))
  @Benchmark def assertHandEqP99EqvP99AP99B: Unit =
                 assert(Hand.EqP100.eqv(p100_a, p100_b))

  val c1_a: C1 = C1.A0(1)
  val c1_b: C1 = C1.A0(1)
  val c2_a: C2 = C2.B0(true)
  val c2_b: C2 = C2.B0(true)
  val c3_a: C3 = C3.S0(1)
  val c3_b: C3 = C3.S0(1)
  val c4_a: C4 = C4.D0(true)
  val c4_b: C4 = C4.D0(true)
  val c5_a: C5 = C5.E0(1)
  val c5_b: C5 = C5.E0(1)
  val c6_a: C6 = C6.F0(true)
  val c6_b: C6 = C6.F0(true)
  val c7_a: C7 = C7.G0(1)
  val c7_b: C7 = C7.G0(1)
  val c8_a: C8 = C8.H0(true)
  val c8_b: C8 = C8.H0(true)
  val c9_a: C9 = C9.I0(1)
  val c9_b: C9 = C9.I0(1)
  val c10_a: C10 = C10.J0(true)
  val c10_b: C10 = C10.J0(true)
  val c20_a: C20 = C20.J1(true)
  val c20_b: C20 = C20.J1(true)
  val c30_a: C30 = C30.J2(true)
  val c30_b: C30 = C30.J2(true)
  val c40_a: C40 = C40.J3(true)
  val c40_b: C40 = C40.J3(true)
  val c50_a: C50 = C50.J4(true)
  val c50_b: C50 = C50.J4(true)
  val c60_a: C60 = C60.J5(true)
  val c60_b: C60 = C60.J5(true)
  val c70_a: C70 = C70.J6(true)
  val c70_b: C70 = C70.J6(true)
  val c80_a: C80 = C80.J7(true)
  val c80_b: C80 = C80.J7(true)
  val c90_a: C90 = C90.J8(true)
  val c90_b: C90 = C90.J8(true)
  val c100_a: C100 = C100.J9(true)
  val c100_b: C100 = C100.J9(true)

  @Benchmark def assertInlinedEqDerivedC01EqvC1AC1B: Unit =
                 assert(inlinedEqDerivedC1.eqv(c1_a, c1_b))
  @Benchmark def assertInlinedEqDerivedC02EqvC2AC2B: Unit =
                 assert(inlinedEqDerivedC2.eqv(c2_a, c2_b))
  @Benchmark def assertInlinedEqDerivedC03EqvC3AC3B: Unit =
                 assert(inlinedEqDerivedC3.eqv(c3_a, c3_b))
  @Benchmark def assertInlinedEqDerivedC04EqvC4AC4B: Unit =
                 assert(inlinedEqDerivedC4.eqv(c4_a, c4_b))
  @Benchmark def assertInlinedEqDerivedC05EqvC5AC5B: Unit =
                 assert(inlinedEqDerivedC5.eqv(c5_a, c5_b))
  @Benchmark def assertInlinedEqDerivedC06EqvC6AC6B: Unit =
                 assert(inlinedEqDerivedC6.eqv(c6_a, c6_b))
  @Benchmark def assertInlinedEqDerivedC07EqvC7AC7B: Unit =
                 assert(inlinedEqDerivedC7.eqv(c7_a, c7_b))
  @Benchmark def assertInlinedEqDerivedC08EqvC8AC8B: Unit =
                 assert(inlinedEqDerivedC8.eqv(c8_a, c8_b))
  @Benchmark def assertInlinedEqDerivedC09EqvC9AC9B: Unit =
                 assert(inlinedEqDerivedC9.eqv(c9_a, c9_b))
  @Benchmark def assertInlinedEqDerivedC10EqvC10AC10B: Unit =
                 assert(inlinedEqDerivedC10.eqv(c10_a, c10_b))
  @Benchmark def assertInlinedEqDerivedC20EqvC20AC20B: Unit =
                 assert(inlinedEqDerivedC20.eqv(c20_a, c20_b))
  @Benchmark def assertInlinedEqDerivedC30EqvC30AC30B: Unit =
                 assert(inlinedEqDerivedC30.eqv(c30_a, c30_b))
  @Benchmark def assertInlinedEqDerivedC40EqvC40AC40B: Unit =
                 assert(inlinedEqDerivedC40.eqv(c40_a, c40_b))
  @Benchmark def assertInlinedEqDerivedC50EqvC50AC50B: Unit =
                 assert(inlinedEqDerivedC50.eqv(c50_a, c50_b))
  @Benchmark def assertInlinedEqDerivedC60EqvC60AC60B: Unit =
                 assert(inlinedEqDerivedC60.eqv(c60_a, c60_b))
  @Benchmark def assertInlinedEqDerivedC70EqvC70AC70B: Unit =
                 assert(inlinedEqDerivedC70.eqv(c70_a, c70_b))
  @Benchmark def assertInlinedEqDerivedC80EqvC80AC80B: Unit =
                 assert(inlinedEqDerivedC80.eqv(c80_a, c80_b))
  @Benchmark def assertInlinedEqDerivedC90EqvC90AC90B: Unit =
                 assert(inlinedEqDerivedC90.eqv(c90_a, c90_b))
  @Benchmark def assertInlinedEqDerivedC99EqvC99AC99B: Unit =
                 assert(inlinedEqDerivedC100.eqv(c100_a, c100_b))

  @Benchmark def assertStagedEqDerivedC01EqvC1AC1B: Unit =
                 assert(stagedEqDerivedC1.eqv(c1_a, c1_b))
  @Benchmark def assertStagedEqDerivedC02EqvC2AC2B: Unit =
                 assert(stagedEqDerivedC2.eqv(c2_a, c2_b))
  @Benchmark def assertStagedEqDerivedC03EqvC3AC3B: Unit =
                 assert(stagedEqDerivedC3.eqv(c3_a, c3_b))
  @Benchmark def assertStagedEqDerivedC04EqvC4AC4B: Unit =
                 assert(stagedEqDerivedC4.eqv(c4_a, c4_b))
  @Benchmark def assertStagedEqDerivedC05EqvC5AC5B: Unit =
                 assert(stagedEqDerivedC5.eqv(c5_a, c5_b))
  @Benchmark def assertStagedEqDerivedC06EqvC6AC6B: Unit =
                 assert(stagedEqDerivedC6.eqv(c6_a, c6_b))
  @Benchmark def assertStagedEqDerivedC07EqvC7AC7B: Unit =
                 assert(stagedEqDerivedC7.eqv(c7_a, c7_b))
  @Benchmark def assertStagedEqDerivedC08EqvC8AC8B: Unit =
                 assert(stagedEqDerivedC8.eqv(c8_a, c8_b))
  @Benchmark def assertStagedEqDerivedC09EqvC9AC9B: Unit =
                 assert(stagedEqDerivedC9.eqv(c9_a, c9_b))
  @Benchmark def assertStagedEqDerivedC10EqvC10AC10B: Unit =
                 assert(stagedEqDerivedC10.eqv(c10_a, c10_b))
  @Benchmark def assertStagedEqDerivedC20EqvC20AC20B: Unit =
                 assert(stagedEqDerivedC20.eqv(c20_a, c20_b))
  @Benchmark def assertStagedEqDerivedC30EqvC30AC30B: Unit =
                 assert(stagedEqDerivedC30.eqv(c30_a, c30_b))
  @Benchmark def assertStagedEqDerivedC40EqvC40AC40B: Unit =
                 assert(stagedEqDerivedC40.eqv(c40_a, c40_b))
  @Benchmark def assertStagedEqDerivedC50EqvC50AC50B: Unit =
                 assert(stagedEqDerivedC50.eqv(c50_a, c50_b))
  @Benchmark def assertStagedEqDerivedC60EqvC60AC60B: Unit =
                 assert(stagedEqDerivedC60.eqv(c60_a, c60_b))
  @Benchmark def assertStagedEqDerivedC70EqvC70AC70B: Unit =
                 assert(stagedEqDerivedC70.eqv(c70_a, c70_b))
  @Benchmark def assertStagedEqDerivedC80EqvC80AC80B: Unit =
                 assert(stagedEqDerivedC80.eqv(c80_a, c80_b))
  @Benchmark def assertStagedEqDerivedC90EqvC90AC90B: Unit =
                 assert(stagedEqDerivedC90.eqv(c90_a, c90_b))
  @Benchmark def assertStagedEqDerivedC99EqvC99AC99B: Unit =
                 assert(stagedEqDerivedC100.eqv(c100_a, c100_b))

  @Benchmark def assertShapeless3EqDerivedC01EqvC1AC1B: Unit =
                 assert(shapeless3EqDerivedC1.eqv(c1_a, c1_b))
  @Benchmark def assertShapeless3EqDerivedC02EqvC2AC2B: Unit =
                 assert(shapeless3EqDerivedC2.eqv(c2_a, c2_b))
  @Benchmark def assertShapeless3EqDerivedC03EqvC3AC3B: Unit =
                 assert(shapeless3EqDerivedC3.eqv(c3_a, c3_b))
  @Benchmark def assertShapeless3EqDerivedC04EqvC4AC4B: Unit =
                 assert(shapeless3EqDerivedC4.eqv(c4_a, c4_b))
  @Benchmark def assertShapeless3EqDerivedC05EqvC5AC5B: Unit =
                 assert(shapeless3EqDerivedC5.eqv(c5_a, c5_b))
  @Benchmark def assertShapeless3EqDerivedC06EqvC6AC6B: Unit =
                 assert(shapeless3EqDerivedC6.eqv(c6_a, c6_b))
  @Benchmark def assertShapeless3EqDerivedC07EqvC7AC7B: Unit =
                 assert(shapeless3EqDerivedC7.eqv(c7_a, c7_b))
  @Benchmark def assertShapeless3EqDerivedC08EqvC8AC8B: Unit =
                 assert(shapeless3EqDerivedC8.eqv(c8_a, c8_b))
  @Benchmark def assertShapeless3EqDerivedC09EqvC9AC9B: Unit =
                 assert(shapeless3EqDerivedC9.eqv(c9_a, c9_b))
  @Benchmark def assertShapeless3EqDerivedC10EqvC10AC10B: Unit =
                 assert(shapeless3EqDerivedC10.eqv(c10_a, c10_b))
  @Benchmark def assertShapeless3EqDerivedC20EqvC20AC20B: Unit =
                 assert(shapeless3EqDerivedC20.eqv(c20_a, c20_b))
  @Benchmark def assertShapeless3EqDerivedC30EqvC30AC30B: Unit =
                 assert(shapeless3EqDerivedC30.eqv(c30_a, c30_b))
  @Benchmark def assertShapeless3EqDerivedC40EqvC40AC40B: Unit =
                 assert(shapeless3EqDerivedC40.eqv(c40_a, c40_b))
  @Benchmark def assertShapeless3EqDerivedC50EqvC50AC50B: Unit =
                 assert(shapeless3EqDerivedC50.eqv(c50_a, c50_b))
  @Benchmark def assertShapeless3EqDerivedC60EqvC60AC60B: Unit =
                 assert(shapeless3EqDerivedC60.eqv(c60_a, c60_b))
  @Benchmark def assertShapeless3EqDerivedC70EqvC70AC70B: Unit =
                 assert(shapeless3EqDerivedC70.eqv(c70_a, c70_b))
  @Benchmark def assertShapeless3EqDerivedC80EqvC80AC80B: Unit =
                 assert(shapeless3EqDerivedC80.eqv(c80_a, c80_b))
  @Benchmark def assertShapeless3EqDerivedC90EqvC90AC90B: Unit =
                 assert(shapeless3EqDerivedC90.eqv(c90_a, c90_b))
  @Benchmark def assertShapeless3EqDerivedC99EqvC99AC99B: Unit =
                 assert(shapeless3EqDerivedC100.eqv(c100_a, c100_b))

  @Benchmark def assertHandEqC1EqvC01AC1B: Unit =
                 assert(Hand.EqC1.eqv(c1_a, c1_b))
  @Benchmark def assertHandEqC2EqvC02AC2B: Unit =
                 assert(Hand.EqC2.eqv(c2_a, c2_b))
  @Benchmark def assertHandEqC3EqvC03AC3B: Unit =
                 assert(Hand.EqC3.eqv(c3_a, c3_b))
  @Benchmark def assertHandEqC4EqvC04AC4B: Unit =
                 assert(Hand.EqC4.eqv(c4_a, c4_b))
  @Benchmark def assertHandEqC5EqvC05AC5B: Unit =
                 assert(Hand.EqC5.eqv(c5_a, c5_b))
  @Benchmark def assertHandEqC6EqvC06AC6B: Unit =
                 assert(Hand.EqC6.eqv(c6_a, c6_b))
  @Benchmark def assertHandEqC7EqvC07AC7B: Unit =
                 assert(Hand.EqC7.eqv(c7_a, c7_b))
  @Benchmark def assertHandEqC8EqvC08AC8B: Unit =
                 assert(Hand.EqC8.eqv(c8_a, c8_b))
  @Benchmark def assertHandEqC9EqvC09AC9B: Unit =
                 assert(Hand.EqC9.eqv(c9_a, c9_b))
  @Benchmark def assertHandEqC10EqvC10AC10B: Unit =
                 assert(Hand.EqC10.eqv(c10_a, c10_b))
  @Benchmark def assertHandEqC20EqvC20AC20B: Unit =
                 assert(Hand.EqC20.eqv(c20_a, c20_b))
  @Benchmark def assertHandEqC30EqvC30AC30B: Unit =
                 assert(Hand.EqC30.eqv(c30_a, c30_b))
  @Benchmark def assertHandEqC40EqvC40AC40B: Unit =
                 assert(Hand.EqC40.eqv(c40_a, c40_b))
  @Benchmark def assertHandEqC50EqvC50AC50B: Unit =
                 assert(Hand.EqC50.eqv(c50_a, c50_b))
  @Benchmark def assertHandEqC60EqvC60AC60B: Unit =
                 assert(Hand.EqC60.eqv(c60_a, c60_b))
  @Benchmark def assertHandEqC70EqvC70AC70B: Unit =
                 assert(Hand.EqC70.eqv(c70_a, c70_b))
  @Benchmark def assertHandEqC80EqvC80AC80B: Unit =
                 assert(Hand.EqC80.eqv(c80_a, c80_b))
  @Benchmark def assertHandEqC90EqvC90AC90B: Unit =
                 assert(Hand.EqC90.eqv(c90_a, c90_b))
  @Benchmark def assertHandEqC99EqvC99AC99B: Unit =
                 assert(Hand.EqC100.eqv(c100_a, c100_b))

  val inc: Int => Int = x => x + 1

  val pk0: PK0[Int] = PK0[Int]()
  val pk1: PK1[Int] = PK1[Int](0)
  val pk2: PK2[Int] = PK2[Int](0, 1)
  val pk3: PK3[Int] = PK3[Int](0, 1, 0)
  val pk4: PK4[Int] = PK4[Int](0, 1, 0, 1)
  val pk5: PK5[Int] = PK5[Int](0, 1, 0, 1, 0)
  val pk6: PK6[Int] = PK6[Int](0, 1, 0, 1, 0, 1)
  val pk7: PK7[Int] = PK7[Int](0, 1, 0, 1, 0, 1, 0)
  val pk8: PK8[Int] = PK8[Int](0, 1, 0, 1, 0, 1, 0, 1)
  val pk9: PK9[Int] = PK9[Int](0, 1, 0, 1, 0, 1, 0, 1, 0)
  val pk10: PK10[Int] = PK10[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk20: PK20[Int] = PK20[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk30: PK30[Int] = PK30[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk40: PK40[Int] = PK40[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk50: PK50[Int] = PK50[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk60: PK60[Int] = PK60[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk70: PK70[Int] = PK70[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk80: PK80[Int] = PK80[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk90: PK90[Int] = PK90[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  val pk100: PK100[Int] = PK100[Int](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)

  val ck1: CK1[Int] = CK1.A0(1)
  val ck2: CK2[Int] = CK2.A0(1)
  val ck3: CK3[Int] = CK3.S0(1)
  val ck4: CK4[Int] = CK4.S0(1)
  val ck5: CK5[Int] = CK5.E0(1)
  val ck6: CK6[Int] = CK6.E0(1)
  val ck7: CK7[Int] = CK7.G0(1)
  val ck8: CK8[Int] = CK8.G0(1)
  val ck9: CK9[Int] = CK9.I0(1)
  val ck10: CK10[Int] = CK10.I0(1)
  val ck20: CK20[Int] = CK20.I1(1)
  val ck30: CK30[Int] = CK30.I2(1)
  val ck40: CK40[Int] = CK40.I3(1)
  val ck50: CK50[Int] = CK50.I4(1)
  val ck60: CK60[Int] = CK60.I5(1)
  val ck70: CK70[Int] = CK70.I6(1)
  val ck80: CK80[Int] = CK80.I7(1)
  val ck90: CK90[Int] = CK90.I8(1)
  val ck100: CK100[Int] = CK100.I9(1)

  @Benchmark def stagedFunctorDerivedCK01MapCK1Inc: Unit =
                 stagedFunctorDerivedCK1.map(ck1)(inc)
  @Benchmark def stagedFunctorDerivedCK02MapCK2Inc: Unit =
                 stagedFunctorDerivedCK2.map(ck2)(inc)
  @Benchmark def stagedFunctorDerivedCK03MapCK3Inc: Unit =
                 stagedFunctorDerivedCK3.map(ck3)(inc)
  @Benchmark def stagedFunctorDerivedCK04MapCK4Inc: Unit =
                 stagedFunctorDerivedCK4.map(ck4)(inc)
  @Benchmark def stagedFunctorDerivedCK05MapCK5Inc: Unit =
                 stagedFunctorDerivedCK5.map(ck5)(inc)
  @Benchmark def stagedFunctorDerivedCK06MapCK6Inc: Unit =
                 stagedFunctorDerivedCK6.map(ck6)(inc)
  @Benchmark def stagedFunctorDerivedCK07MapCK7Inc: Unit =
                 stagedFunctorDerivedCK7.map(ck7)(inc)
  @Benchmark def stagedFunctorDerivedCK08MapCK8Inc: Unit =
                 stagedFunctorDerivedCK8.map(ck8)(inc)
  @Benchmark def stagedFunctorDerivedCK09MapCK9Inc: Unit =
                 stagedFunctorDerivedCK9.map(ck9)(inc)
  @Benchmark def stagedFunctorDerivedCK10MapCK10Inc: Unit =
                 stagedFunctorDerivedCK10.map(ck10)(inc)
  @Benchmark def stagedFunctorDerivedCK20MapCK20Inc: Unit =
                 stagedFunctorDerivedCK20.map(ck20)(inc)
  @Benchmark def stagedFunctorDerivedCK30MapCK30Inc: Unit =
                 stagedFunctorDerivedCK30.map(ck30)(inc)
  @Benchmark def stagedFunctorDerivedCK40MapCK40Inc: Unit =
                 stagedFunctorDerivedCK40.map(ck40)(inc)
  @Benchmark def stagedFunctorDerivedCK50MapCK50Inc: Unit =
                 stagedFunctorDerivedCK50.map(ck50)(inc)
  @Benchmark def stagedFunctorDerivedCK60MapCK60Inc: Unit =
                 stagedFunctorDerivedCK60.map(ck60)(inc)
  @Benchmark def stagedFunctorDerivedCK70MapCK70Inc: Unit =
                 stagedFunctorDerivedCK70.map(ck70)(inc)
  @Benchmark def stagedFunctorDerivedCK80MapCK80Inc: Unit =
                 stagedFunctorDerivedCK80.map(ck80)(inc)
  @Benchmark def stagedFunctorDerivedCK90MapCK90Inc: Unit =
                 stagedFunctorDerivedCK90.map(ck90)(inc)
  @Benchmark def stagedFunctorDerivedCK99MapCK99Inc: Unit =
                 stagedFunctorDerivedCK100.map(ck100)(inc)

  @Benchmark def stagedFunctorDerivedPK00MapPK0Inc: Unit =
                 stagedFunctorDerivedPK0.map(pk0)(inc)
  @Benchmark def stagedFunctorDerivedPK01MapPK1Inc: Unit =
                 stagedFunctorDerivedPK1.map(pk1)(inc)
  @Benchmark def stagedFunctorDerivedPK02MapPK2Inc: Unit =
                 stagedFunctorDerivedPK2.map(pk2)(inc)
  @Benchmark def stagedFunctorDerivedPK03MapPK3Inc: Unit =
                 stagedFunctorDerivedPK3.map(pk3)(inc)
  @Benchmark def stagedFunctorDerivedPK04MapPK4Inc: Unit =
                 stagedFunctorDerivedPK4.map(pk4)(inc)
  @Benchmark def stagedFunctorDerivedPK05MapPK5Inc: Unit =
                 stagedFunctorDerivedPK5.map(pk5)(inc)
  @Benchmark def stagedFunctorDerivedPK06MapPK6Inc: Unit =
                 stagedFunctorDerivedPK6.map(pk6)(inc)
  @Benchmark def stagedFunctorDerivedPK07MapPK7Inc: Unit =
                 stagedFunctorDerivedPK7.map(pk7)(inc)
  @Benchmark def stagedFunctorDerivedPK08MapPK8Inc: Unit =
                 stagedFunctorDerivedPK8.map(pk8)(inc)
  @Benchmark def stagedFunctorDerivedPK09MapPK9Inc: Unit =
                 stagedFunctorDerivedPK9.map(pk9)(inc)
  @Benchmark def stagedFunctorDerivedPK10MapPK10Inc: Unit =
                 stagedFunctorDerivedPK10.map(pk10)(inc)
  @Benchmark def stagedFunctorDerivedPK20MapPK20Inc: Unit =
                 stagedFunctorDerivedPK20.map(pk20)(inc)
  @Benchmark def stagedFunctorDerivedPK30MapPK30Inc: Unit =
                 stagedFunctorDerivedPK30.map(pk30)(inc)
  @Benchmark def stagedFunctorDerivedPK40MapPK40Inc: Unit =
                 stagedFunctorDerivedPK40.map(pk40)(inc)
  @Benchmark def stagedFunctorDerivedPK50MapPK50Inc: Unit =
                 stagedFunctorDerivedPK50.map(pk50)(inc)
  @Benchmark def stagedFunctorDerivedPK60MapPK60Inc: Unit =
                 stagedFunctorDerivedPK60.map(pk60)(inc)
  @Benchmark def stagedFunctorDerivedPK70MapPK70Inc: Unit =
                 stagedFunctorDerivedPK70.map(pk70)(inc)
  @Benchmark def stagedFunctorDerivedPK80MapPK80Inc: Unit =
                 stagedFunctorDerivedPK80.map(pk80)(inc)
  @Benchmark def stagedFunctorDerivedPK90MapPK90Inc: Unit =
                 stagedFunctorDerivedPK90.map(pk90)(inc)
  @Benchmark def stagedFunctorDerivedPK99MapPK99Inc: Unit =
                 stagedFunctorDerivedPK100.map(pk100)(inc)

  @Benchmark def shapeless3FunctorDerivedCK01MapCK1Inc: Unit =
                 shapeless3FunctorDerivedCK1.map(ck1)(inc)
  @Benchmark def shapeless3FunctorDerivedCK02MapCK2Inc: Unit =
                 shapeless3FunctorDerivedCK2.map(ck2)(inc)
  @Benchmark def shapeless3FunctorDerivedCK03MapCK3Inc: Unit =
                 shapeless3FunctorDerivedCK3.map(ck3)(inc)
  @Benchmark def shapeless3FunctorDerivedCK04MapCK4Inc: Unit =
                 shapeless3FunctorDerivedCK4.map(ck4)(inc)
  @Benchmark def shapeless3FunctorDerivedCK05MapCK5Inc: Unit =
                 shapeless3FunctorDerivedCK5.map(ck5)(inc)
  @Benchmark def shapeless3FunctorDerivedCK06MapCK6Inc: Unit =
                 shapeless3FunctorDerivedCK6.map(ck6)(inc)
  @Benchmark def shapeless3FunctorDerivedCK07MapCK7Inc: Unit =
                 shapeless3FunctorDerivedCK7.map(ck7)(inc)
  @Benchmark def shapeless3FunctorDerivedCK08MapCK8Inc: Unit =
                 shapeless3FunctorDerivedCK8.map(ck8)(inc)
  @Benchmark def shapeless3FunctorDerivedCK09MapCK9Inc: Unit =
                 shapeless3FunctorDerivedCK9.map(ck9)(inc)
  @Benchmark def shapeless3FunctorDerivedCK10MapCK10Inc: Unit =
                 shapeless3FunctorDerivedCK10.map(ck10)(inc)
  @Benchmark def shapeless3FunctorDerivedCK20MapCK20Inc: Unit =
                 shapeless3FunctorDerivedCK20.map(ck20)(inc)
  @Benchmark def shapeless3FunctorDerivedCK30MapCK30Inc: Unit =
                 shapeless3FunctorDerivedCK30.map(ck30)(inc)
  @Benchmark def shapeless3FunctorDerivedCK40MapCK40Inc: Unit =
                 shapeless3FunctorDerivedCK40.map(ck40)(inc)
  @Benchmark def shapeless3FunctorDerivedCK50MapCK50Inc: Unit =
                 shapeless3FunctorDerivedCK50.map(ck50)(inc)
  @Benchmark def shapeless3FunctorDerivedCK60MapCK60Inc: Unit =
                 shapeless3FunctorDerivedCK60.map(ck60)(inc)
  @Benchmark def shapeless3FunctorDerivedCK70MapCK70Inc: Unit =
                 shapeless3FunctorDerivedCK70.map(ck70)(inc)
  @Benchmark def shapeless3FunctorDerivedCK80MapCK80Inc: Unit =
                 shapeless3FunctorDerivedCK80.map(ck80)(inc)
  @Benchmark def shapeless3FunctorDerivedCK90MapCK90Inc: Unit =
                 shapeless3FunctorDerivedCK90.map(ck90)(inc)
  @Benchmark def shapeless3FunctorDerivedCK99MapCK99Inc: Unit =
                 shapeless3FunctorDerivedCK100.map(ck100)(inc)

  @Benchmark def shapeless3FunctorDerivedPK00MapPK0Inc: Unit =
                 shapeless3FunctorDerivedPK0.map(pk0)(inc)
  @Benchmark def shapeless3FunctorDerivedPK01MapPK1Inc: Unit =
                 shapeless3FunctorDerivedPK1.map(pk1)(inc)
  @Benchmark def shapeless3FunctorDerivedPK02MapPK2Inc: Unit =
                 shapeless3FunctorDerivedPK2.map(pk2)(inc)
  @Benchmark def shapeless3FunctorDerivedPK03MapPK3Inc: Unit =
                 shapeless3FunctorDerivedPK3.map(pk3)(inc)
  @Benchmark def shapeless3FunctorDerivedPK04MapPK4Inc: Unit =
                 shapeless3FunctorDerivedPK4.map(pk4)(inc)
  @Benchmark def shapeless3FunctorDerivedPK05MapPK5Inc: Unit =
                 shapeless3FunctorDerivedPK5.map(pk5)(inc)
  @Benchmark def shapeless3FunctorDerivedPK06MapPK6Inc: Unit =
                 shapeless3FunctorDerivedPK6.map(pk6)(inc)
  @Benchmark def shapeless3FunctorDerivedPK07MapPK7Inc: Unit =
                 shapeless3FunctorDerivedPK7.map(pk7)(inc)
  @Benchmark def shapeless3FunctorDerivedPK08MapPK8Inc: Unit =
                 shapeless3FunctorDerivedPK8.map(pk8)(inc)
  @Benchmark def shapeless3FunctorDerivedPK09MapPK9Inc: Unit =
                 shapeless3FunctorDerivedPK9.map(pk9)(inc)
  @Benchmark def shapeless3FunctorDerivedPK10MapPK10Inc: Unit =
                 shapeless3FunctorDerivedPK10.map(pk10)(inc)
  @Benchmark def shapeless3FunctorDerivedPK20MapPK20Inc: Unit =
                 shapeless3FunctorDerivedPK20.map(pk20)(inc)
  @Benchmark def shapeless3FunctorDerivedPK30MapPK30Inc: Unit =
                 shapeless3FunctorDerivedPK30.map(pk30)(inc)
  @Benchmark def shapeless3FunctorDerivedPK40MapPK40Inc: Unit =
                 shapeless3FunctorDerivedPK40.map(pk40)(inc)
  @Benchmark def shapeless3FunctorDerivedPK50MapPK50Inc: Unit =
                 shapeless3FunctorDerivedPK50.map(pk50)(inc)
  @Benchmark def shapeless3FunctorDerivedPK60MapPK60Inc: Unit =
                 shapeless3FunctorDerivedPK60.map(pk60)(inc)
  @Benchmark def shapeless3FunctorDerivedPK70MapPK70Inc: Unit =
                 shapeless3FunctorDerivedPK70.map(pk70)(inc)
  @Benchmark def shapeless3FunctorDerivedPK80MapPK80Inc: Unit =
                 shapeless3FunctorDerivedPK80.map(pk80)(inc)
  @Benchmark def shapeless3FunctorDerivedPK90MapPK90Inc: Unit =
                 shapeless3FunctorDerivedPK90.map(pk90)(inc)
  @Benchmark def shapeless3FunctorDerivedPK99MapPK99Inc: Unit =
                 shapeless3FunctorDerivedPK100.map(pk100)(inc)

  @Benchmark def handFunctorCK01MapCK1Inc: Unit =
                 Hand.FunctorCK1.map(ck1)(inc)
  @Benchmark def handFunctorCK02MapCK2Inc: Unit =
                 Hand.FunctorCK2.map(ck2)(inc)
  @Benchmark def handFunctorCK03MapCK3Inc: Unit =
                 Hand.FunctorCK3.map(ck3)(inc)
  @Benchmark def handFunctorCK04MapCK4Inc: Unit =
                 Hand.FunctorCK4.map(ck4)(inc)
  @Benchmark def handFunctorCK05MapCK5Inc: Unit =
                 Hand.FunctorCK5.map(ck5)(inc)
  @Benchmark def handFunctorCK06MapCK6Inc: Unit =
                 Hand.FunctorCK6.map(ck6)(inc)
  @Benchmark def handFunctorCK07MapCK7Inc: Unit =
                 Hand.FunctorCK7.map(ck7)(inc)
  @Benchmark def handFunctorCK08MapCK8Inc: Unit =
                 Hand.FunctorCK8.map(ck8)(inc)
  @Benchmark def handFunctorCK09MapCK9Inc: Unit =
                 Hand.FunctorCK9.map(ck9)(inc)
  @Benchmark def handFunctorCK10MapCK10Inc: Unit =
                 Hand.FunctorCK10.map(ck10)(inc)
  @Benchmark def handFunctorCK20MapCK20Inc: Unit =
                 Hand.FunctorCK20.map(ck20)(inc)
  @Benchmark def handFunctorCK30MapCK30Inc: Unit =
                 Hand.FunctorCK30.map(ck30)(inc)
  @Benchmark def handFunctorCK40MapCK40Inc: Unit =
                 Hand.FunctorCK40.map(ck40)(inc)
  @Benchmark def handFunctorCK50MapCK50Inc: Unit =
                 Hand.FunctorCK50.map(ck50)(inc)
  @Benchmark def handFunctorCK60MapCK60Inc: Unit =
                 Hand.FunctorCK60.map(ck60)(inc)
  @Benchmark def handFunctorCK70MapCK70Inc: Unit =
                 Hand.FunctorCK70.map(ck70)(inc)
  @Benchmark def handFunctorCK80MapCK80Inc: Unit =
                 Hand.FunctorCK80.map(ck80)(inc)
  @Benchmark def handFunctorCK90MapCK90Inc: Unit =
                 Hand.FunctorCK90.map(ck90)(inc)
  @Benchmark def handFunctorCK99MapCK99Inc: Unit =
                 Hand.FunctorCK100.map(ck100)(inc)

  @Benchmark def handFunctorPK00MapPK0Inc: Unit =
                 Hand.FunctorPK0.map(pk0)(inc)
  @Benchmark def handFunctorPK01MapPK1Inc: Unit =
                 Hand.FunctorPK1.map(pk1)(inc)
  @Benchmark def handFunctorPK02MapPK2Inc: Unit =
                 Hand.FunctorPK2.map(pk2)(inc)
  @Benchmark def handFunctorPK03MapPK3Inc: Unit =
                 Hand.FunctorPK3.map(pk3)(inc)
  @Benchmark def handFunctorPK04MapPK4Inc: Unit =
                 Hand.FunctorPK4.map(pk4)(inc)
  @Benchmark def handFunctorPK05MapPK5Inc: Unit =
                 Hand.FunctorPK5.map(pk5)(inc)
  @Benchmark def handFunctorPK06MapPK6Inc: Unit =
                 Hand.FunctorPK6.map(pk6)(inc)
  @Benchmark def handFunctorPK07MapPK7Inc: Unit =
                 Hand.FunctorPK7.map(pk7)(inc)
  @Benchmark def handFunctorPK08MapPK8Inc: Unit =
                 Hand.FunctorPK8.map(pk8)(inc)
  @Benchmark def handFunctorPK09MapPK9Inc: Unit =
                 Hand.FunctorPK9.map(pk9)(inc)
  @Benchmark def handFunctorPK10MapPK10Inc: Unit =
                 Hand.FunctorPK10.map(pk10)(inc)
  @Benchmark def handFunctorPK20MapPK20Inc: Unit =
                 Hand.FunctorPK20.map(pk20)(inc)
  @Benchmark def handFunctorPK30MapPK30Inc: Unit =
                 Hand.FunctorPK30.map(pk30)(inc)
  @Benchmark def handFunctorPK40MapPK40Inc: Unit =
                 Hand.FunctorPK40.map(pk40)(inc)
  @Benchmark def handFunctorPK50MapPK50Inc: Unit =
                 Hand.FunctorPK50.map(pk50)(inc)
  @Benchmark def handFunctorPK60MapPK60Inc: Unit =
                 Hand.FunctorPK60.map(pk60)(inc)
  @Benchmark def handFunctorPK70MapPK70Inc: Unit =
                 Hand.FunctorPK70.map(pk70)(inc)
  @Benchmark def handFunctorPK80MapPK80Inc: Unit =
                 Hand.FunctorPK80.map(pk80)(inc)
  @Benchmark def handFunctorPK90MapPK90Inc: Unit =
                 Hand.FunctorPK90.map(pk90)(inc)
  @Benchmark def handFunctorPK99MapPK99Inc: Unit =
                 Hand.FunctorPK100.map(pk100)(inc)
}
