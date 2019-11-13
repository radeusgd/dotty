import scala.tasty.TypeTest

sealed trait Foo {

  type X
  type Y <: X

  def x: X

  def f(y: Y) = println("ok")

  given TypeTest[X, Y] = new TypeTest {
    def isInstance(x: X): TypeTest.Result[x.type & Y] =
      TypeTest.success(x.asInstanceOf[x.type & Y])
  }

  object Z {
    def unapply(arg: Y): Option[Int] = Some(9)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    test(new Foo { type X = Int; type Y = Int; def x: X = 1 })
  }

  def test(foo: Foo): Unit = {
    import foo.given
    foo.x match {
      case x @ foo.Z(i) => // `x` is refined to type `foo.Y`
        foo.f(x)
        println(i)
    }
  }
}
