import scala.quoted._

import Monoid._

object Test {
  implicit def toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  import SD._
  import delegate SD._

  def main(args: Array[String]): Unit = run {
    println("hello")
    def printApp[T: Type](sd: StaDyn[T]) given Monoid[Expr[T]]: Expr[Unit] =
      '{ println(${cd(sd).show.toExpr}); println(${cd(sd)}); println() }

    '{
      val x: Int = 5
      ${ printApp(sta(3) * sta(2) * sta(2)) }
      ${ val xx = 'x; printApp(dyn(xx) * dyn(xx) * dyn(xx) * dyn(xx) * dyn(xx) * dyn(xx) * dyn(xx) * dyn(xx)) }
      ${ val xx = 'x; printApp(sta(3) * dyn(xx) * sta(2) * sta(3) * dyn(xx) * dyn(xx) * dyn(xx) * dyn(xx) * dyn(xx) * sta(1)) }
    }
  }
}

class Bag[T](private[Bag] val mp: Map[T, Int]) {
  def toList: List[(T, Int)] = mp.toList
  def union(that: Bag[T]): Bag[T] = new Bag(mp.transform((x, n) => n + that.mp.getOrElse(x, 0)) ++ that.mp.filter(x => !mp.contains(x._1)))
}

object Bag {
  def empty[T]: Bag[T] = new Bag(Map.empty)
  def sigleton[T](x: T): Bag[T] = new Bag(Map(x -> 1))
}

trait GenStaDyn[T, Sta[_], Dyn[_]] {
  val sta: Sta[T]
  val dyn: Dyn[T]
}
type BagOfExpr[X] = Bag[Expr[X]]
case class StaDyn[T](sta: Option[T], dyn: Bag[Expr[T]]) extends GenStaDyn[T, Option, BagOfExpr]

object SD {
  // type Bag[T] = Map[T, Int]

  def sta[T: Monoid: Liftable](i: T) given QuoteContext: StaDyn[T] =
    StaDyn(Some(i), Bag.sigleton(i.toExpr))

  def dyn[T](i: Expr[T]) given Monoid[Expr[T]]: StaDyn[T] =
    StaDyn(None, Bag.sigleton(i))

  def dyn[T](bag: Bag[Expr[T]]) given Monoid[Bag[Expr[T]]]: StaDyn[T] =
    StaDyn(None, bag)

  def cd[T: Type](x: StaDyn[T]) given (m: Monoid[Expr[T]]) given QuoteContext: Expr[T] = {
    def pow(x: Expr[T], n: Int): Expr[T] =
      if (n == 0) m.one
      else if (n == 1) x // Avoid the extra 1
      else if (n == 2) x * x // Avoid the extra val
      else if (n % 2 == 0) '{ val y = ${m.prod(x, x)}; ${pow('y, n / 2)}  }
      else m.prod(x, pow(x, n - 1))
    x._2.toList.map((x, i) => pow(x, i)).reduce((a, b) => m.prod(a, b))

  }

  delegate [T: Type: Liftable] for Monoid[StaDyn[T]] given (m: Monoid[T], m2: Monoid[Bag[Expr[T]]]) given QuoteContext {
    val one = StaDyn(Some(`1`), `1`)
    val prod = (x, y) => (x.sta, y.sta) match {
      case (Some(a), Some(b)) => sta(a * b)
      case (Some(m.one), _) => y
      case (_, Some(m.one)) => x
      case _ => dyn(x._2 * y._2)
    }
  }


}

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

trait CMonoid[X] extends Monoid[X]

