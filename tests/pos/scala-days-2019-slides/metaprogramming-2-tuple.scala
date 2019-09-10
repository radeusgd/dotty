object TupleExample {
  import Tuple._

  trait A
  trait B
  trait C

  the[Concat[A *: B *: Unit, C *: Unit]    =:=    A *: B *: C *: Unit]

  the[Concat[A *: B *: Unit, C *: Tuple]   =:=    A *: B *: C *: Tuple]

  the[Concat[A *: B *: Tuple, C *: Unit]   <:<    A *: B *: Tuple]
}
