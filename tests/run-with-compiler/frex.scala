import scala.quoted._

import Monoid._
import Semigroup._

object Test {
  implicit def toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  import StaDyn._
  import delegate StaDyn._

  def main(args: Array[String]): Unit = run {
    println("hello")

    def printApp(sd: StaDyn[Int, BagOfExpr]): Expr[Unit] =
      '{ println(${sd.code.show.toExpr}); println(${sd.code}); println() }

    def printAppNat(sd: StaDyn[Nat.T, BagOfExpr]): Expr[Unit] =
      '{ println(${sd.code.show.toExpr}); println(${sd.code}); println() }

    '{
      val x = Nat(5)
      ${ printApp(sta[Int, BagOfExpr](3) * sta[Int, BagOfExpr](2) * sta[Int, BagOfExpr](2)) }
      ${ val xx = Bag.sigleton('x); printApp(dyn(xx)) }
      ${ val xx = Bag.sigleton('x); printApp(dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx)) }
      ${ val xx = Bag.sigleton('x); printApp(sta[Int, BagOfExpr](3) * dyn[Int, BagOfExpr](xx) * sta[Int, BagOfExpr](2) * sta[Int, BagOfExpr](3) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * dyn[Int, BagOfExpr](xx) * sta[Int, BagOfExpr](1)) }
    }
  }
}

object Nat {
  opaque type T = Int
  def apply(n: Int): T = n
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

trait Semigroup[X] {
  val prod : (X, X) => X
}
object Semigroup {
  def (a: T) * [T](b: T): given Semigroup[T] => T = the[Semigroup[T]].prod(a, b)
}

trait CSemigroup[X] extends Semigroup[X]

trait Monoid[X] extends Semigroup[X] {
  val one: X
}
object Monoid {
  def `1`[T]: given Monoid[T] => T = the[Monoid[T]].one
}

trait CMonoid[X] extends Monoid[X] with CSemigroup[X]

delegate for CMonoid[Int] {
  val one = 1
  val prod = (x, y) => x * y
}

delegate for CMonoid[Expr[Int]] given QuoteContext {
  val one = '{1}
  val prod = (x, y) => '{ $x * $y }
}

delegate for CMonoid[Bag[Expr[Int]]] given QuoteContext {
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
// SEMI
//

trait Semi[T]
case class LeafS[T](value: T) extends Semi[T]
case class LeafD[T](value: Expr[T]) extends Semi[T]
case class ConsS[T](value: T, tail: Semi[T]) extends Semi[T]
case class ConsD[T](expr: Expr[T], tail: Semi[T]) extends Semi[T]

object Semi {
  def consS[T: Semigroup](h: T, tail: Semi[T]): Semi[T] = tail match {
    case LeafS(v) => LeafS(h * v)
    case ConsS(v, tail) => ConsS(h * v, tail)
    case _ => ConsS(h, tail)
  }

  def consD[T](h: Expr[T], tail: Semi[T]): Semi[T] =
    ConsD(h, tail)
}

delegate [T: Semigroup] for Semigroup[Semi[T]] {
  val prod = (x, y) => x match {
    case LeafS(v) => Semi.consS(v, y)
    case LeafD(e) => Semi.consD(e, y)
    case ConsS(v, t) => Semi.consS(v, t * y)
    case ConsD(e, t) => Semi.consD(e, t * y)
  }
}
