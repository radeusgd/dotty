object TupleExample {
  import Tuple._

  type A
  type B
  type C

  the[Concat[A *: B *: Tuple0, C *: Tuple0]    =:=    A *: B *: C *: Tuple0]

  the[Concat[A *: B *: Tuple0, C *: Tuple]   =:=    A *: B *: C *: Tuple]

  the[Concat[A *: B *: Tuple, C *: Tuple0]   <:<    A *: B *: Tuple]
}
