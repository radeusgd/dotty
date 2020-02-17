package foo

import scala.tasty._
import scala.deriving._
import scala.quoted.{Expr => E, _}
import scala.annotation.tailrec
import scala.compiletime._

type QE[X] = QuoteContext ?=> E[X]
type Id[t] = t
type Const[c] = [t] =>> c

implicit class ListOps[A](l: List[A]) {
  def safeZip[B](o: List[B]): List[(A, B)] = {
    assert(l.size == o.size, s"Zipping $l with $o :/")
    l.zip(o)
  }
}

// Third party library
object K0 {
  class SummonInstances[F[_], T](val instances: List[Any])
  object SummonInstances {
    implicit def caseNil[F[_]]: SummonInstances[F, Unit] =
      new SummonInstances[F, Unit](Nil)

    implicit def caseCons[F[_], H, T <: Tuple](implicit h: F[H], t: SummonInstances[F, T]): SummonInstances[F, H *: T] =
      new SummonInstances[F, H *: T](h :: t.instances)
  }

  trait StagedInstances[F[_], T] {}

  trait StagedProductInstances[F[_], T] extends StagedInstances[F, T] {
    def instances: List[Any]
    def accessorsE(value: E[T])(implicit q: QuoteContext): List[E[Any]]
    def constructorE(fields: List[E[Any]])(implicit q: QuoteContext): E[T]

    def foldLeft2E[Acc](x: E[T], y: E[T])(i: E[Acc])(f: [t] => (E[Acc], F[t], E[t], E[t]) => E[Acc]): QE[Acc] =
      accessorsE(x).safeZip(accessorsE(y)).safeZip(instances).foldLeft(i) {
        case (acc, ((xn, yn), in)) => f(acc, in.asInstanceOf, xn, yn)
      }
  }

  object StagedProductInstances {
    implicit def apply[F[_], T]
      (given m: Mirror.ProductOf[T])
      (given
        t: Type[T],
        s: SummonInstances[F, m.MirroredElemTypes]
      ): StagedProductInstances[F, T] = new StagedProductInstances[F, T] {
        def instances: List[Any] = s.instances
        def accessorsE(value: E[T])(implicit q: QuoteContext): List[E[Any]] = {
          import q.tasty.{_, given}
          t.unseal.symbol match {
            case sym if sym.isClassDef =>
              sym.caseFields.map { field =>
                Select.unique(value.unseal, field.name).seal
              }
            case _ =>
              Nil // case object
          }
        }

        def constructorE(fields: List[E[Any]])(implicit q: QuoteContext): E[T] = {
          import q.tasty.{_, given}
          def companion(tpe: Type): TermRef = tpe match {
            case t @ TermRef(_, _)     => t
            case TypeRef(name, prefix) => TermRef(name, prefix)
            case TypeLambda(_, _, AppliedType(tcons, _)) => companion(tcons)
          }
          Select.overloaded(Ident(companion(t.unseal.tpe)), "apply", Nil, fields.map(_.unseal)).seal.cast[T]
        }
      }
  }

  trait StagedCoproductInstances[F[_], T] extends StagedInstances[F, T] {
    def instances: List[Any]
    def typetestsE(value: E[T])(implicit q: QuoteContext): List[E[Boolean]]
    def castsE(value: E[T])(implicit q: QuoteContext): List[E[Any]]

    def fold2E[R: Type](x: E[T], y: E[T])(i: E[R])(f: [t] => (F[t], E[t], E[t]) => E[R]): QE[R] =
      typetestsE(x)
        .safeZip(castsE(x))
        .safeZip(
          typetestsE(y).safeZip(castsE(y))
        ).safeZip(instances).foldLeft(i) {
        case (acc, (((tx, cx), (ty, cy)), in)) =>
          '{ if ($tx && $ty) ${ f(in.asInstanceOf, cx, cy) } else $acc }
      }
  }

  object StagedCoproductInstances {
    implicit def apply[F[_], T]
      (given m: Mirror.SumOf[T])
      (given
        t: Type[T],
        s: SummonInstances[F, m.MirroredElemTypes],
        x: SummonInstances[Type, m.MirroredElemTypes],
      ): StagedCoproductInstances[F, T] = new StagedCoproductInstances[F, T] {
        def instances: List[Any] = s.instances

        def typetestsE(value: E[T])(implicit q: QuoteContext): List[E[Boolean]] =
          x.instances.map { case tpe: Type[_] => '{ $value.isInstanceOf[$tpe] } }

        def castsE(value: E[T])(implicit q: QuoteContext): List[E[Any]] = {
          x.instances.map { case tpe: Type[_] => '{ $value.asInstanceOf[$tpe] } }
        }
      }
  }
}

// Type class definitions

trait Eq[A] {
  def eqv(x: A, y: A): Boolean
}

case class StagedInstance[F[_], A](underlying: Any)
object StagedInstance {
  implicit def product[F[_], A](implicit inst: K0.StagedProductInstances[F, A]): StagedInstance[F, A] =
    StagedInstance[F, A](inst)
  implicit def coproduct[F[_], A](implicit inst: K0.StagedCoproductInstances[F, A]): StagedInstance[F, A] =
    StagedInstance[F, A](inst)
}

object Eq {
  inline def derived[T](implicit inline e: => StagedInstance[Eq0, T]): Eq[T] =
    e.underlying match {
      case inst: K0.StagedProductInstances[Eq0, T] =>
        new Eq[T] {
          def eqv(a: T, b: T): Boolean = eqStaged(a, b)(inst)
        }
      case inst: K0.StagedCoproductInstances[Eq0, T] =>
        new Eq[T] {
          def eqv(a: T, b: T): Boolean = eqStaged(a, b)(inst)
        }
    }

  // implicit def eqGenP[A](implicit inst: K0.StagedProductInstances[Eq0, A]): Eq0[A] =
  //   new Eq0[A] {
  //     def eqv(x: E[A], y: E[A]): QE[Boolean] =
  //       inst.foldLeft2E(x, y)('{ true })(
  //         [t] => (acc: E[Boolean], eqt: Eq0[t], t0: E[t], t1: E[t]) =>
  //           '{ $acc && ${ eqt.eqv(t0, t1) }}
  //       )
  //   }

  // implicit def eqGenC[A](implicit inst: K0.StagedCoproductInstances[Eq0, A]): Eq0[A] =
  //   new Eq0[A] {
  //     def eqv(x: E[A], y: E[A]): QE[Boolean] = inst.fold2E(x, y)('{ false })(
  //       [t] => (eqt: Eq0[t], t0: E[t], t1: E[t]) => eqt.eqv(t0, t1)
  //     )
  //   }

  // inline def derived[T](implicit inline e: => Eq0[T]): Eq[T] =
  //   new Eq[T] {
  //     def eqv(a: T, b: T): Boolean = eqStaged(a, b)
  //   }

  inline def eqStaged[T](f1: T, f2: T)(implicit inline e: => Eq0[T]): Boolean =
    ${ eqStagedImpl('f1, 'f2, e) }

  inline def eqStaged[T](f1: T, f2: T)(implicit inline e: => Eq0[T]): Boolean =
    ${ eqStagedImpl('f1, 'f2, e) }

  var foo = 1
  def eqStagedImpl[T](f1: E[T], f2: E[T], e: Eq0[T])(implicit q: QuoteContext): E[Boolean] = {
    foo += 1
    e.eqv(f1, f2)
  }
}

trait Eq0[A] {
  def eqv(x: E[A], y: E[A]): QE[Boolean]
}

object Eq0 {
  implicit def eqUnit: Eq0[Unit] = new Eq0[Unit] {
    def eqv(x: E[Unit], y: E[Unit]): QE[Boolean] =
      '{ true }
  }

  implicit def eqBoolean: Eq0[Boolean] = new Eq0[Boolean] {
    def eqv(x: E[Boolean], y: E[Boolean]): QE[Boolean] =
      '{ $x == $y }
  }

  implicit def eqInt: Eq0[Int] = new Eq0[Int] {
    def eqv(x: E[Int], y: E[Int]): QE[Boolean] =
      '{ $x == $y }
  }

  implicit def eqString: Eq0[String] = new Eq0[String] {
    def eqv(x: E[String], y: E[String]): QE[Boolean] =
      '{ $x == $y }
  }

  implicit def eqGenP[A](implicit inst: K0.StagedProductInstances[Eq0, A]): Eq0[A] =
    new Eq0[A] {
      def eqv(x: E[A], y: E[A]): QE[Boolean] =
        inst.foldLeft2E(x, y)('{ true })(
          [t] => (acc: E[Boolean], eqt: Eq0[t], t0: E[t], t1: E[t]) =>
            '{ $acc && ${ eqt.eqv(t0, t1) }}
        )
    }

  implicit def eqGenC[A](implicit inst: K0.StagedCoproductInstances[Eq0, A]): Eq0[A] =
    new Eq0[A] {
      def eqv(x: E[A], y: E[A]): QE[Boolean] = inst.fold2E(x, y)('{ false })(
        [t] => (eqt: Eq0[t], t0: E[t], t1: E[t]) => eqt.eqv(t0, t1)
      )
    }

  implicit def eil0: Eq0[IList] = new Eq0[IList] {
    def eqv(x: E[IList], y: E[IList]): QE[Boolean] = '{Test.eil.eqv($x, $y)}
  }
}

// trait Generic[T] {
//   type Repr
//   def from(r: Repr): T
//   def to(t: T): Repr
// }

case class ISB(i: Int, s: String, b: Boolean)

sealed trait OptionInt // derives Eq
case class SomeInt(value: Int) extends OptionInt
case object NoneInt extends OptionInt

object OptionInt

sealed trait IList
case class ICons(hd: Int, tl: IList) extends IList
case object INil extends IList

object IList

case class Box[A](x: A) // derives Functor

sealed trait Opt[+A] // derives Functor
case class Sm[+A](value: A) extends Opt[A]
case object Nn extends Opt[Nothing]

object Opt

sealed trait CList[+A] // derives Functor
case class CCons[+A](hd: A, tl: CList[A]) extends CList[A]
case object CNil extends CList[Nothing]

object CList
