import Hand.{Eq, Functor}
  object FunctorCK4 extends Functor[CK4] {
    import CK4._
    def map[A, B](fa: CK4[A])(f: A => B): CK4[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x)
    }
  } // FunctorCK4
