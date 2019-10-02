import scala.quoted._

private object TestHoles {
  // val x: Int = ?foo
  // val y: String = ?foo
  List(1,2,3).map(?f)
  List("foo").map(?f)
}
