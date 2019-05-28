import Function._

object Test extends dotty.runtime.LegacyApp {
  def tuple(n: Int): NonEmptyTuple = // TODO return Tuple
    1 *: Tuple.fromArray((2 to n).toArray)

  for (n <- 1 to 25) {
    val tup = tuple(n)
    assert(tup.productArity == n)
    assert(tup.productElement(0) == 1)
    assert(tup.productElement(n - 1) == n)
  }
}
