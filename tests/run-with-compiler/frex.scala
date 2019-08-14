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
    x.toList.map((x, i) => pow(x, i)).reduce((a, b) => m.prod(a, b))
  }
}

delegate [T] for Lift[T, T] = (x: T) => x
delegate [T, U] for Lift[T, Bag[U]] given Monoid[U] given (lift: Lift[T, U]) = (x: T) => Bag.sigleton(lift(x))
delegate [T: Liftable] for Lift[T, Expr[T]] given QuoteContext = (x: T) => x.toExpr

//
// STATIC DYNAMIC
//

sealed trait StaDyn[S, D[S]] {
  def dyn: D[S]
  def code given (lift: Lift[D[S], Expr[S]]): Expr[S] = lift(dyn)
}
final case class Sta[S, D[S]](sta: S)(val dyn: D[S]) extends StaDyn[S, D]
final case class Dyn[S, D[S]](dyn: D[S]) extends StaDyn[S, D]

object StaDyn {
  def sta[S, D[_]](i: S) given (lift: Lift[S, D[S]]): StaDyn[S, D] = Sta(i)(lift(i))
  def dyn[S, D[_]](d: D[S]): StaDyn[S, D] = Dyn(d)
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
    case (Sta(a), Sta(b)) => StaDyn.sta(a * b)
    case (Sta(sm.one), y) => y
    case (x, Sta(sm.one)) => x
    case (x, y) => StaDyn.dyn(x.dyn * y.dyn)
  }
}

//
// CMONOIDS
//

trait CMonoid[X] extends Monoid[X]

