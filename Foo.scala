import quoted._
object Foo {
  def whileNotZero(body: Expr[Int] => Expr[Unit]) = ???
//  '{
//    var x = 0
//    while (x == 0) ~body('(x))
//  }
  whileNotZero(x => '{ ~x = 1 })
}
