object IndirectValueOf {
  def indirectValueOf[T]: T = Foo.valueOf[T]
}

