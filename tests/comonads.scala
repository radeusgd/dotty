import scala.reflect.ClassTag

// https://github.com/dorchard/codo-notation/blob/master/arrays.lhs

object Test {

  trait Functor[F[_]] {
    def map[A, B](x: F[A])(f: A => B): F[B]
  }

  trait Comonad[C[_]] extends Functor[C] {
    def extract[A](c: C[A]): A
    def extend[A, B](c: C[A] => B): C[A] => C[B]
  }

  trait IArray[A](
    arr: Array[A],
    i: Int,
    indices: List[Int],
    assoc: List[(Int, A)])

  given as Comonad[IArray] {

    def extract[A](c: IArray[A]): A = c.arr(c.i)

    def extend[A, B: ClassTag](f: IArray[A] => B): IArray[A] => IArray[B] = (c: IArray[A]) => {
      var es = c.arr.indices.map(i => (i, f(IArray(c.arr, c.i, c.indices)))).toArray[B]

      IArray[B](es, c.i, c.indices, es)
    }
  }
}