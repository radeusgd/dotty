import Function._

object Test extends dotty.runtime.LegacyApp {
  def tuple(n: Int): Tuple =
    Tuple.fromArray((1 to n).toArray)

  for (n <- 0 to 25) {
    val tup = tuple(n)
    assert(tup.productArity == n)
    assert(tup.productElement(0) == 1)
    assert(tup.productElement(n - 1) == n)
  }
}
