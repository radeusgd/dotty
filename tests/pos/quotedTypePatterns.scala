object Test {

  def res[T](t: quoted.Type[T]) given tasty.Reflection: Unit = t match {
    case '[Double] =>
    case _ =>
  }

}
