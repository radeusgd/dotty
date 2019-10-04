import scala.quoted._
import scala.collection.mutable

private object TestHoles {
  // val x: Int = ?foo
  // val y: String = ?foo

  // List(1,2,3).map(?f)


  // List("foo").map(?f)

  // val x = mutable.ListBuffer[?A]()
  // x += 1
  // x += "hi"
  def foo: Unit = {
    // var x: ?A = 1
    // x = ""

    val xs = List(1, 2, 3)
    // ?A >: List[B]
    // B >: Int <: Any

    // xs.foldLeft[?A](Nil)((acc, x) => x :: acc)

    // xs.foldLeft[?A](Nil)((acc, x) => acc.::[Int](x))
  }
}
