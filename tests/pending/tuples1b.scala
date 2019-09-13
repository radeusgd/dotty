object Test extends App {
  // This match type reduction is sound but currently not unsupported.
  // The current implementation refuses to bind a type variable `x` in
  // a match type pattern `P[x]` if the scrutinee `S` type contains
  // any abstraction. In this case, we are matching `T *: T *: Unit`
  // with `h *: _`, so we are not "decomposing" on an abstract type,
  // i.e. `*:` is concrete, but detecting that would require duplicating
  // some of the logic from type comparer.

  def x[T](xs: (T, T)): T = xs.head
  def x1[T](xs: (T, T)): Tuple1[T] = xs.tail
  def x2[T](xs: (T, T)): T = xs(0)
  def x3[T](xs: (T, T)): Unit = xs(0)

  println(x((0,1)))
  println(x1((0,1)))
  println(x2((0,1)))
  println(x3((0,1)))
}
