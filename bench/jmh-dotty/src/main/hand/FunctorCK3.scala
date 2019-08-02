import Hand.{Eq, Functor}
  object FunctorCK3 extends Functor[CK3] {
    import CK3._
    def map[A, B](fa: CK3[A])(f: A => B): CK3[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x))
    }
  } // FunctorCK3
