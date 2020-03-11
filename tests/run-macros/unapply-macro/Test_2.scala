
@main def Test = {
  "foo" match
    case ReversedFoo(x) => assert(x == "oof")

  "bar" match
    case ReversedFoo(x) => assert(false, x)
    case _ => // ok

}
