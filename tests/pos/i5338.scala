object Test {
  val t0 = Tuple0()

  def f[T <: Tuple, X](xs: T, x: X) =
    xs ++ x *: t0
}
