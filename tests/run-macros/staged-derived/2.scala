object Test {
  def fooEq(a: Foo, b: Foo): Boolean = {
    import Eq0Macro._
    Eq0Macro.derived[Foo].e(a, b)
  }

  def barEq(a: Bar, b: Bar): Boolean = {
    import Eq0Macro._
    Eq0Macro.derived[Bar].e(a, b)
  }

  def main(args: Array[String]): Unit = {
    assert( fooEq(Foo(1, "s"), Foo(1, "s")))
    assert(!fooEq(Foo(2, "s"), Foo(1, "s")))
    assert(!fooEq(Foo(1, "Z"), Foo(1, "s")))

    assert( barEq(Bur(1), Bur(1)))
    assert(!barEq(Bur(2), Bur(1)))
    assert( barEq(Bor("s"), Bor("s")))
    assert(!barEq(Bor("T"), Bor("s")))
    assert(!barEq(Bur(1), Bor("s")))
  }
}
