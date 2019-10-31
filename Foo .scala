import compiletime.summonFrom

object Foo {
  inline def valueOf[T]: T = summonFrom {
    case ev: ValueOf[T] => ev.value
  }
}
