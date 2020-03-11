trait X[+T]
trait A[+F[+_]]

@main def Test: Unit = {
  println(test[X[Int]])
  println(test[A[X]])
}
