import scala.reflect.ClassTag


object Test {

  trait Functor[F[_]] {
    def map[A, B](x: F[A])(f: A => B): F[B]
  }

  trait Comonad[C[_]] extends Functor[C] {
    def extract[A](c: C[A]): A
    def extend[A, B](c: C[A] => B): C[A] => C[B]
  }

  case class CArray[A](a: Array[A], i: Int)

  given as Comonad[CArray] {
    var cursor = 0

    def extract[A](c: CArray[A]): A = c.a(cursor)

    def extend[A, B: ClassTag](f: CArray[A] => B): CArray[A] => CArray[B] = (c: CArray[A]) => {
      val es = (0 until c.a.size).map(j => f(CArray(c.a, j)))

      CArray[B](es.toArray[B], c.i)
    }
  }
}