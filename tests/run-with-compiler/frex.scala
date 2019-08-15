import scala.quoted._

import Monoid._

object Test {
  implicit def toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  import StaDyn._
  import delegate StaDyn._

  def main(args: Array[String]): Unit = run {
    println("hello")

    def printApp(sd: StaDyn[Int, BagOfExpr]): Expr[Unit] =
      '{ println(${sd.code.show.toExpr}); println(${sd.code}); println() }

    '{
      val x: Int = 5
      ${ printApp(sta[Int, BagOfExpr](3) * sta[Int, BagOfExpr](2) * sta[Int, BagOfExpr](2)) }
      ${ val xx = Bag.sigleton('x); printApp(dyn(xx)) }
      ${ val xx = Bag.sigleton('x); printApp(dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx)) }
      ${ val xx = Bag.sigleton('x); printApp(sta[Int, BagOfExpr](3) * dyn[Int, BagOfExpr](xx) * sta[Int, BagOfExpr](2) * sta[Int, BagOfExpr](3) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * sta[Int, BagOfExpr](1)) }
    }
  }
}

//
// BAG
//

class Bag[T](private[Bag] val mp: Map[T, Int]) {
  def isEmpty = mp.isEmpty
  def toList: List[(T, Int)] = mp.toList
  def union(that: Bag[T]): Bag[T] = new Bag(mp.transform((x, n) => n + that.mp.getOrElse(x, 0)) ++ that.mp.filter(x => !mp.contains(x._1)))
}

object Bag {
  def empty[T]: Bag[T] = new Bag(Map.empty)
  def sigleton[T](x: T): Bag[T] = new Bag(Map(x -> 1))
}

type BagOfExpr[X] = Bag[Expr[X]]

//
// LIFT
//

trait Lift[S, D] {
  def apply(s: S): D
}

delegate [T: Type] for Lift[Bag[Expr[T]], Expr[T]] given (m: Monoid[Expr[T]]) given QuoteContext {
  def apply(x: Bag[Expr[T]]): Expr[T] = {
    def pow(x: Expr[T], n: Int): Expr[T] =
      if (n == 0) m.one
      else if (n == 1) x // Avoid the extra 1
      else if (n == 2) x * x // Avoid the extra val
      else if (n % 2 == 0) '{ val y = ${m.prod(x, x)}; ${pow('y, n / 2)}  }
      else m.prod(x, pow(x, n - 1))
    if (x.isEmpty) m.one
    else x.toList.map((x, i) => pow(x, i)).reduce((a, b) => m.prod(a, b))
  }
}

delegate [T] for Lift[T, T] = (x: T) => x
delegate [T, U] for Lift[T, Bag[U]] given Monoid[U] given (lift: Lift[T, U]) = (x: T) => Bag.sigleton(lift(x))
delegate [T: Liftable] for Lift[T, Expr[T]] given QuoteContext = (x: T) => x.toExpr

//
// STATIC DYNAMIC
//

// type BindingTime
// type Sta <: BindingTime
// type Dyn <: BindingTime

// trait BT[X <: BindingTime]
// object BTSta extends BT[Sta]
// object BTDyn extends BT[Dyn]

// trait SD[T <: BindingTime, A]
// case class S[A](v: A) extends SD[Sta, A]
// case class D[A](v: Expr[A]) extends SD[Dyn, A]

// trait Mag[T <: BindingTime, A]
// case class Leaf[T, A](v: SD[Sta, A]) extends Mag[T, A]
// case class Br1[A](left: Mag[Sta, A], right: Mag[Dyn, A]) extends Mag[Dyn, A]
// case class Br2[A](left: Mag[Dyn, A], right: Mag[Sta, A]) extends Mag[Dyn, A]

// enum Tree[A] {
//   case Leaf(v: A)
//   case Branch(left: Tree[A], right: Tree[A])
// }

// trait Magma[A] {
//   val mag: (A, A) => A
// }

// delegate [A] for Magma[Tree[A]] {
//   val mag = Tree.Branch(_, _)
// }

// delegate [A] for Magma[Exists[Mag, A]] {
//   val mag: (A, A) => A = ???
// }

// instance Magma a ⇒ Magma (Exists Mag a) where
// E a • E b = case (btMag a, btMag b, a, b) of
// (BTSta, BTSta, LeafM (S a), LeafM (S b)) → E (LeafM (S (a • b)))
// (BTSta, BTDyn, l, r) → E (Br1 l r)
// (BTDyn, _ , l, r) → E (Br2 l r)

case class Exists[A[_, _], B](v: B)
// data Exists (f :: k1 →k2 →*) a where E :: f b a → Exists f a

sealed trait StaDyn[S, D[S]] {
  def dyn: D[S]
  def code given (lift: Lift[D[S], Expr[S]]): Expr[S] = lift(dyn)
}
final case class Sta2[S, D[S]](sta: S)(val dyn: D[S]) extends StaDyn[S, D]
final case class Dyn2[S, D[S]](dyn: D[S]) extends StaDyn[S, D]

object StaDyn {
  def sta[S, D[S]](i: S) given (lift: Lift[S, D[S]]): StaDyn[S, D] = Sta2(i)(lift(i))
  def dyn[S, D[S]](d: D[S]): StaDyn[S, D] = Dyn2(d)
}

//
// MONOIDS
//

trait Monoid[X] {
  val one: X
  val prod : (X, X) => X
}
object Monoid {
  def `1`[T]: given Monoid[T] => T = the[Monoid[T]].one
  def (a: T) * [T](b: T): given Monoid[T] => T = the[Monoid[T]].prod(a, b)
}

delegate for Monoid[Int] {
  val one = 1
  val prod = (x, y) => x * y
}

delegate for Monoid[Expr[Int]] given QuoteContext {
  val one = '{1}
  val prod = (x, y) => '{ $x * $y }
}

delegate for Monoid[Bag[Expr[Int]]] given QuoteContext {
  val one = Bag.empty
  val prod = (x, y) => x.union(y)
}

delegate [S, D[_]] for Monoid[StaDyn[S, D]] given (sm: Monoid[S], dm: Monoid[D[S]], lift: Lift[S, D[S]]) given QuoteContext {
  val one = StaDyn.sta(`1`)
  val prod = (x, y) => (x, y) match {
    case (Sta2(a), Sta2(b)) => StaDyn.sta(a * b)
    case (Sta2(sm.one), y) => y
    case (x, Sta2(sm.one)) => x
    case (x, y) => StaDyn.dyn(x.dyn * y.dyn)
  }
}

//
// CMONOIDS
//

trait CMonoid[X] extends Monoid[X]

//
// MAGMA
//

// enum Mag[S, D[S]] {
//   case Leaf(v: StaDyn[S, D])
//   case Br1(left: Mag[S, D], right: Mag[S, D])
// }

//
// ALTERNATE
//

trait Alternate[X, Y]
case class Empty[A, B]() extends Alternate[A, B]
case class ConsA[A, B](head: A, tail: ConsB[A, B] | Empty[A, B]) extends Alternate[A, B]
case class ConsB[A, B](head: B, tail: ConsA[A, B] | Empty[A, B]) extends Alternate[A, B]

