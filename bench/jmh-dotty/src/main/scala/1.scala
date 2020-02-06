package scala

import scala.tasty._
import scala.deriving._
import scala.quoted.{Expr => E, _}
import scala.annotation.tailrec
import scala.compiletime._

object Staged {
  type RE[X] = QuoteContext ?=> E[X]
  type Id[t] = t
  type Const[c] = [t] =>> c

  implicit class ListOps[A](l: List[A]) {
    def safeZip[B](o: List[B]): List[(A, B)] = {
      assert(l.size == o.size, s"Zipping $l with $o :/")
      l zip o
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
    // { def map(x: T)(f: [t] => (F[t], t) => t): T }

    trait StagedProductInstances[F[_], T] extends StagedInstances[F, T] {
      // implicit val tag: Type[T]
      def instances: List[Any]
      def accessorsE(value: E[T])(implicit q: QuoteContext): List[E[Any]]
      def constructorE(fields: List[E[Any]])(implicit q: QuoteContext): E[T]

      def foldLeft2E[Acc](x: E[T], y: E[T])(i: E[Acc])(f: [t] => (E[Acc], F[t], E[t], E[t]) => E[Acc]): RE[Acc] =
        accessorsE(x).safeZip(accessorsE(y)).safeZip(instances).foldLeft(i) {
          case (acc, ((xn, yn), in)) => f(acc, in.asInstanceOf, xn, yn)
        }

      // def foldLeft2E[Acc: Type](x0: E[T], y0: E[T])(i: E[Acc])(f: [t] => (E[Acc], F[t], E[t], E[t]) => E[Acc]): RE[Acc] = {
      //   def re(x: E[T], y: E[T]) =
      //     accessorsE(x).safeZip(accessorsE(y)).safeZip(instances).foldLeft(i) {
      //       case (acc, ((xn, yn), in)) =>
      //         f(acc, in.asInstanceOf, xn, yn)
      //     }
      //   // val instanceOrRecurse = null
      //   '{
      //     def foo(x: T, y: T) =
      //       ${ re('x, 'y) }
      //     foo($x0, $y0)
      //   }
      //    //   def merge(a: E[T], b: E[T]): E[Acc] = {
      //    //     $re
      //    //   }
      //    //   ${ merge(x, y) }
      //    // }
      // }
    }

    object StagedProductInstances {
      implicit def apply[F[_], T]
        (using m: Mirror.ProductOf[T])
        (using
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes]
        ): StagedProductInstances[F, T] = new StagedProductInstances[F, T] {
          // val tag = t

          def instances: List[Any] = s.instances

          def accessorsE(value: E[T])(implicit q: QuoteContext): List[E[Any]] = {
            import q.tasty.{_, given}
            import q.tasty.QuotedTypeAPI.unseal
            unseal(t).symbol match {
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
            import q.tasty.QuotedTypeAPI.unseal
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

      def fold2E[R: Type](x: E[T], y: E[T])(i: E[R])(f: [t] => (F[t], E[t], E[t]) => E[R]): RE[R] =
        typetestsE(x).safeZip(castsE(x)).safeZip(typetestsE(y).safeZip(castsE(y))).safeZip(instances).foldLeft(i) {
          case (acc, (((tx, cx), (ty, cy)), in)) =>
            '{ if ($tx && $ty) ${ f(in.asInstanceOf, cx, cy) } else $acc }
        }
    }

    object StagedCoproductInstances {
      implicit def apply[F[_], T]
        (using m: Mirror.SumOf[T])
        (using
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

  object K1 {
    case class Wrap[T](t: T)
    class Dummy
    type Apply[T[_]] = T[Dummy]
    type Unapply[F[_[_]], T] = T match {
      case Wrap[Apply[a]] => F[a]
      case Wrap[Dummy] => F[Id]
      case Wrap[c] => F[Const[c]]
    }

    type ProductGeneric[O[_]] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes[_] }
    type CoproductGeneric[O[_]] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes[_] }

    class SummonInstances[F[_[_]], G[_]](val instances: List[Any])
    object SummonInstances {
      implicit def applied[F[_[_]], T[_]](implicit s: SummonInstances0[F, Apply[T]]): SummonInstances[F, T] = {
        // assert(s.instances.nonEmpty)
        new SummonInstances[F, T](s.instances)
      }
    }

    class SummonInstances0[F[_[_]], T](val instances: List[Any])
    object SummonInstances0 {
      implicit def caseNil[F[_[_]]]: SummonInstances0[F, Unit] =
        new SummonInstances0[F, Unit](Nil)

      implicit def caseCons[F[_[_]], H, T <: Tuple]
        (implicit
          h: Unapply[F, Wrap[H]],
          t: SummonInstances0[F, T]
        ): SummonInstances0[F, H *: T] =
          new SummonInstances0[F, H *: T](h :: t.instances)
    }

    trait StagedInstances[F[_[_]], T[_]] {
      def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]]
    }

    object StagedInstances {
      implicit def fromStagedProductInstances[F[_[_]], T[_]]
        (implicit spi: StagedProductInstances[F, T]): StagedInstances[F, T] = spi

      implicit def fromStagedCoproductInstances[F[_[_]], T[_]]
        (implicit spi: StagedCoproductInstances[F, T]): StagedInstances[F, T] = spi
    }

    trait StagedProductInstances[F[_[_]], T[_]] extends StagedInstances[F, T] {
      def instances: List[Any]
      def accessorsE[A](value: E[T[A]])(implicit q: QuoteContext): List[E[Any]]
      def constructorE[A](fields: List[E[Any]])(implicit q: QuoteContext): E[T[A]]

      def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]] = {
        val args = instances.safeZip(accessorsE(x)).map((in, a) => f(in.asInstanceOf, a.asInstanceOf))
        constructorE(args)
      }
    }

    object StagedProductInstances {
      implicit def apply[F[_[_]], T[_]]
        (using m: ProductGeneric[T])
        (using
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes]
        ): StagedProductInstances[F, T] = new StagedProductInstances[F, T] {
          def instances: List[Any] = s.instances

          def accessorsE[A](value: E[T[A]])(implicit q: QuoteContext): List[E[Any]] = {
            import q.tasty.{_, given}
            QuotedTypeAPI.unseal(t).symbol match {
              case sym if sym.isClassDef =>
                sym.caseFields.map { field =>
                  Select.unique(value.unseal, field.name).seal
                }
              case s if s.isNoSymbol =>
                t.unseal.tpe match {
                  case TypeLambda(_, _, AppliedType(tcons, _)) =>
                    tcons.classSymbol match {
                      case Some(sym) if sym.isClassDef =>
                        sym.caseFields.map { field =>
                          Select.unique(value.unseal, field.name).seal
                        }
                      case _ => Nil
                    }
                  case _ => Nil
                }
            }
          }

          def constructorE[A](fields: List[E[Any]])(implicit q: QuoteContext): E[T[A]] = {
            import q.tasty.{_, given}
            def companion(tpe: Type): TermRef = tpe match {
              case t @ TermRef(_, _)     => t
              case TypeRef(name, prefix) => TermRef(name, prefix)
              case TypeLambda(_, _, AppliedType(tcons, _)) => companion(tcons)
            }
            def defaultTargs(tpe: Type): List[Type] = tpe match {
              case TermRef(_, _)   => Nil
              case TypeRef(_, _) => Nil
              case TypeLambda(_, bounds, _) =>
                // Do we need to call .low for covariant cases? I don't think we do...
                bounds.map(_.hi)
            }
            val t0 = QuotedTypeAPI.unseal(t).tpe
            Select.overloaded(Ident(companion(t0)), "apply", defaultTargs(t0), fields.map(_.unseal))
              .seal.asInstanceOf[E[T[A]]]
          }
        }
    }

    trait StagedCoproductInstances[F[_[_]], T[_]] extends StagedInstances[F, T] {
      def instances: List[Any]
      def typetestsE[A](value: E[T[A]])(implicit q: QuoteContext): List[E[Boolean]]
      def castsE[A](value: E[T[A]])(implicit q: QuoteContext): List[E[Any]]

      def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]] =
        instances.safeZip(typetestsE(x).safeZip(castsE(x))).foldLeft('{ null }) {
          case (acc, (i, (t, c))) => '{ if ($t) ${ f(i.asInstanceOf, c.asInstanceOf) } else $acc }
        }.asInstanceOf
    }

    object StagedCoproductInstances {
      implicit def apply[F[_[_]], T[_]]
        (using m: CoproductGeneric[T])
        (using
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes],
          x: SummonInstances[Type, m.MirroredElemTypes],
        ): StagedCoproductInstances[F, T] = new StagedCoproductInstances[F, T] {
          def instances: List[Any] = s.instances

          def typetestsE[A](value: E[T[A]])(implicit q: QuoteContext): List[E[Boolean]] =
            x.instances.map { case tpe: Type[_] =>
              '{ ${ value.asInstanceOf[E[T[Any]]] }.isInstanceOf[quoted.Type[Any]] }
            }

          def castsE[A](value: E[T[A]])(implicit q: QuoteContext): List[E[Any]] =
            x.instances.map { case tpe: Type[_] =>
              import q.tasty.{_, given}
              import q.tasty.QuotedTypeAPI.unseal
              import q.tasty.QuotedTypeAPI.unseal
              tpe.unseal.tpe match {
                case TypeLambda(_, bound :: Nil, _) =>
                  '{ ${ value.asInstanceOf[E[T[Any]]] }.asInstanceOf[quoted.Type[Any]] }
              }
            }
        }
      }
  }
  // Type class definitions

  trait Eq[A] {
    def eqv(x: A, y: A): Boolean
  }

  object Eq {
    inline def derived[T](implicit inline e: => Eq0[T]): Eq[T] =
      new Eq[T] {
        def eqv(a: T, b: T): Boolean = eqStaged(a, b)
      }

    inline def eqStaged[T](f1: T, f2: T)(implicit inline e: => Eq0[T]): Boolean =
      ${ eqStagedImpl('f1, 'f2, e) }

    def eqStagedImpl[T](f1: E[T], f2: E[T], e: Eq0[T])(implicit q: QuoteContext): E[Boolean] =
      e.eqv(f1, f2)
  }

  trait Eq0[A] {
    def eqv(x: E[A], y: E[A]): RE[Boolean]
  }

  object Eq0 {
    implicit def eqUnit: Eq0[Unit] = new Eq0[Unit] {
      def eqv(x: E[Unit], y: E[Unit]): RE[Boolean] =
        '{ true }
    }

    implicit def eqBoolean: Eq0[Boolean] = new Eq0[Boolean] {
      def eqv(x: E[Boolean], y: E[Boolean]): RE[Boolean] =
        '{ $x == $y }
    }

    implicit def eqInt: Eq0[Int] = new Eq0[Int] {
      def eqv(x: E[Int], y: E[Int]): RE[Boolean] =
        '{ $x == $y }
    }

    implicit def eqString: Eq0[String] = new Eq0[String] {
      def eqv(x: E[String], y: E[String]): RE[Boolean] =
        '{ $x == $y }
    }

    implicit def eqGenP[A](implicit inst: K0.StagedProductInstances[Eq0, A]): Eq0[A] =
      new Eq0[A] {
        def eqv(x: E[A], y: E[A]): RE[Boolean] =
          inst.foldLeft2E(x, y)('{ true })(
            [t] => (acc: E[Boolean], eqt: Eq0[t], t0: E[t], t1: E[t]) =>
              '{ $acc && ${ eqt.eqv(t0, t1) }}
          )
      }

    implicit def eqGenC[A](implicit inst: K0.StagedCoproductInstances[Eq0, A]): Eq0[A] =
      new Eq0[A] {
        def eqv(x: E[A], y: E[A]): RE[Boolean] = inst.fold2E(x, y)('{ false })(
          [t] => (eqt: Eq0[t], t0: E[t], t1: E[t]) => eqt.eqv(t0, t1)
        )
      }
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    inline def derived[F[_]](implicit inline e: => Functor0[F]): Functor[F] =
      new Functor[F] {
        def map[A, B](fa: F[A])(f: A => B): F[B] = mapStaged(fa, f)
      }

    inline def mapStaged[F[_], A, B](fa: F[A], f: A => B)(implicit inline e: => Functor0[F]): F[B] =
      ${ mapStagedImpl('fa, 'f)(e) }

    def mapStagedImpl[F[_], A: Type, B: Type](fa: E[F[A]], f: E[A => B])(e: Functor0[F])(implicit q: QuoteContext): E[F[B]] =
      e.map(fa)(f)
  }

  trait Functor0[F[_]] {
    def map[A: Type, B: Type](fa: E[F[A]])(f: E[A => B]): RE[F[B]]
  }

  object Functor0 {
    implicit def functorId: Functor0[Id] = new Functor0[Id] {
      def map[A: Type, B: Type](a: E[A])(f: E[A => B]): RE[B] = '{ $f($a) }
    }

    implicit def functorConst[T]: Functor0[Const[T]] = new Functor0[Const[T]] {
      def map[A: Type, B: Type](t: E[T])(f: E[A => B]): RE[T] = t
    }

    implicit def functorNested[F[_]: Type, G[_]: Type](implicit ff: Functor0[F], fg: Functor0[G]): Functor0[[t] =>> F[G[t]]] =
      new Functor0[[t] =>> F[G[t]]] {
        def map[A: Type, B: Type](fga: E[F[G[A]]])(f: E[A => B]): RE[F[G[B]]] =
          ff.map(fga)('{ ga => ${ fg.map('{ ga })(f) } })
          // ff.map[G[A], G[B]](fga)('{ (ga: G[A]) => ${ fg.map[A, B]('{ ga })(f) } })
      }

    implicit def derived[F[_]](implicit inst: K1.StagedInstances[Functor0, F]): Functor0[F] =
      new Functor0[F] {
        def map[A: Type, B: Type](fa: E[F[A]])(f: E[A => B]): RE[F[B]] =
          inst.mapE(fa)([t[_]] => (ft: Functor0[t], ta: E[t[A]]) => ft.map(ta)(f))
      }
  }
}

// ADTs

case class ISB(i: Int, s: String, b: Boolean) // derives Eq

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
