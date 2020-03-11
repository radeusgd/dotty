import scala.quoted._

object ReversedFoo {

  inline def unapply(x: String): Option[String] =
    if x == "foo" then Some("off") else None

}


@main def Test = {
  "foo" match
    case ReversedFoo(x) => assert(x == "oof")

  // "bar" match
  //   case ReversedFoo(x) => assert(false, x)
  //   case _ => // ok

}
