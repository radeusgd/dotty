class Parent(x: Int) {
  var name: String = _
  var addr: String = _

  val len = name.size                        // error: name not initialized
  lazy val l1 = name.size                    // ok: l1 is lazy
  lazy val l2 = addr.size                    // error: l2 is forced at L32 before `addr` is initialized

  val fun: Int => Int = n => n + list.size   // ok, fun is a cold value
  val bar: Bar = new Bar("bar", fun)         // ok, Bar accepts cold value
  bar.result                                 // error: bar depends on list

  val child = new Child(this)                // error: `this` is cold, while full value expected
  List(5, 9).map(n => n + list.size)         // error: cold value used as full value

  f(20)                                      // error: list not initialized

  val list = List(1, 2, 3)

  if (x > 5) {
    name = "big"
    addr = "Lausanne"
  }
  else {
    name = "small"
  }

  val temp1 = l1                               // ok, name init
  val temp2 = l2                               // error: addr not initialized

  private def f(m: Int) =
    m + list.size                              // error: `f` is called at L19 before `list` is initialized

  List(1, 3, 5).map(n => n + list.size)        // ok, `this.list` already initialized
}


class Bar(val name: String, fun: Cold[Int => Int]) {
  def result = fun(20)
}

class Child(parent: Parent) {
  println(parent.name)
}