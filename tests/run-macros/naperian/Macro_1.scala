import scala.quoted._

// interface Functor f => Naperian (f : Type -> Type) where
//   Log : {f : Type} -> Type
//   lookup' : f a -> (Log -> a)
//   positions : f Log
//   tabulate : (Log -> a) -> f a

object Naperians {
  trait Functor[F[_]] {
    def [A, B](x: F[A]) map (f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def [A, B](x: F[A]) flatMap (f: A => F[B]): F[B]
    def [A, B](x: F[A]) map (f: A => B) = x.flatMap(f `andThen` pure)

    def pure[A](x: A): F[A]
  }

  trait Naperian[F[_]] {
    type Log

    def [A](x: F[A]) lookup(l: Log): A

    def positions: F[Log]
    def tabulate[A](f: Log => A): F[A]
  }

  // instance Naperian Pair where
  //   type Log Pair = Bool
  //   lookup (P x y) b = if b then y else x
  //   positions = P False True

  given listMonad as Monad[List] {
    def [A, B](xs: List[A]) flatMap (f: A => List[B]): List[B] = xs.flatMap(f)
    def pure[A](x: A): List[A] = List(x)
  }

  type Pair = [A] =>> (A, A)

  given naperianTuple as Naperian[Pair] {
    type Log = Boolean

    def [A](x: Pair[A]) lookup(l: Log): A = if l then x._1 else x._2

    def positions: Pair[Log] = (true, false)

    def tabulate[A](f: Log => A): Pair[A] = (f(true), f(false))
  }

  given naperianArray as Naperian[Array] {
    type Log = Int

    def [A](x: Array[A]) lookup(l: Log): A = x(l)

    def positions: Array[Log] = (0 until 9).toArray

    def tabulate[A](f: Log => A): Array[A] = ??? // (0 until 9).map(i => f(i)).toArray
  }

}