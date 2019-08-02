import Hand.{Eq, Functor}
  object FunctorCK1 extends Functor[CK1] {
    import CK1._
    def map[A, B](fa: CK1[A])(f: A => B): CK1[B] = fa match {
      case A0(x) => A0(f(x))
    }
  } // FunctorCK1
