import scala.tasty._
import scala.deriving._
import scala.quoted.{Expr => E, _}
import scala.annotation.tailrec

object Inlined {
  trait Eq[T] {
    def eql(x: T, y: T): Boolean
  }

  object Eq {
    import scala.compiletime.erasedValue
    import compiletime._
    import scala.deriving._

    inline def tryEql[TT](x: TT, y: TT): Boolean = delegate match {
      case eq: Eq[TT] => eq.eql(x, y)
    }

    inline def eqlElems[Elems <: Tuple](n: Int)(x: Any, y: Any): Boolean =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          tryEql[elem](productElement[elem](x, n), productElement[elem](y, n)) &&
          eqlElems[elems1](n + 1)(x, y)
        case _: Unit =>
          true
      }

    inline def eqlProduct[T](m: Mirror.ProductOf[T])(x: Any, y: Any): Boolean =
      eqlElems[m.MirroredElemTypes](0)(x, y)

    inline def eqlCases[Alts](n: Int)(x: Any, y: Any, ord: Int): Boolean =
      inline erasedValue[Alts] match {
        case _: (alt *: alts1) =>
          if (ord == n)
            delegate match {
              case m: Mirror.ProductOf[`alt`] => eqlElems[m.MirroredElemTypes](0)(x, y)
            }
          else eqlCases[alts1](n + 1)(x, y, ord)
        case _: Unit =>
          false
      }

    inline def derived[T](implicit ev: Mirror.Of[T]): Eq[T] = new Eq[T] {
      def eql(x: T, y: T): Boolean =
        inline ev match {
          case m: Mirror.SumOf[T] =>
            val ord = m.ordinal(x)
            ord == m.ordinal(y) && eqlCases[m.MirroredElemTypes](0)(x, y, ord)
          case m: Mirror.ProductOf[T] =>
            eqlElems[m.MirroredElemTypes](0)(x, y)
        }
    }

    implicit object IntEq extends Eq[Int] {
      def eql(x: Int, y: Int) = x == y
    }

    implicit object BooleanEq extends Eq[Boolean] {
      def eql(x: Boolean, y: Boolean) = x == y
    }
  }
}

object Staged {
  type RE[X] = given Reflection => E[X]

  implicit class ListOps[A](l: List[A]) {
    def safeZip[B](o: List[B]): List[(A, B)] = {
      assert(l.size == o.size)
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

    trait StagedInstances[F[_], T]
    // { def map(x: T)(f: [t] => (F[t], t) => t): T }

    trait StagedProductInstances[F[_], T] extends StagedInstances[F, T] {
      def instances: List[Any]
      def accessorsE(value: E[T])(implicit r: Reflection): List[E[Any]]
      def constructorE(fields: List[E[Any]])(implicit r: Reflection): E[T]

      def foldLeft2E[Acc](x: E[T], y: E[T])(i: E[Acc])(f: [t] => (E[Acc], F[t], E[t], E[t]) => E[Acc]): RE[Acc] =
        accessorsE(x).safeZip(accessorsE(y)).safeZip(instances).foldLeft(i) {
          case (acc, ((xn, yn), in)) => f(acc, in.asInstanceOf, xn, yn)
        }
    }

    object StagedProductInstances {
      implicit def apply[F[_], T]
        (implicit
          m: Mirror.ProductOf[T],
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes]
        ): StagedProductInstances[F, T] = new StagedProductInstances[F, T] {
          def instances: List[Any] = s.instances

          def accessorsE(value: E[T])(implicit r: Reflection): List[E[Any]] = {
            import r._
            t.unseal.symbol match {
              case IsClassDefSymbol(self) => // case class
                self.caseFields.map { field =>
                  Select.unique(value.unseal, field.name).seal
                }
              case _ => Nil // case object
            }
          }

          def constructorE(fields: List[E[Any]])(implicit r: Reflection): E[T] = {
            import r._
            val companion = t.unseal.tpe match {
              case Type.SymRef(sym, prefix)   => Type.TermRef(prefix, sym.name)
              case Type.TypeRef(name, prefix) => Type.TermRef(prefix, name)
            }
            Select.overloaded(Ident(companion), "apply", Nil, fields.map(_.unseal)).seal.cast[T]
          }
        }
    }

    trait StagedCoproductInstances[F[_], T] extends StagedInstances[F, T] {
      def instances: List[Any]
      def typetestsE(value: E[T])(implicit r: Reflection): List[E[Boolean]]
      def castsE(value: E[T])(implicit r: Reflection): List[E[Any]]

      def fold2E[R: Type](x: E[T], y: E[T])(i: E[R])(f: [t] => (F[t], E[t], E[t]) => E[R]): RE[R] =
        typetestsE(x).safeZip(castsE(x)).safeZip(typetestsE(y).safeZip(castsE(y))).safeZip(instances).foldLeft(i) {
          case (acc, (((tx, cx), (ty, cy)), in)) =>
            '{ if ($tx && $ty) ${ f(in.asInstanceOf, cx, cy) } else $acc }
        }
    }

    object StagedCoproductInstances {
      implicit def apply[F[_], T]
        (implicit
          m: Mirror.SumOf[T],
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes],
          x: SummonInstances[Type, m.MirroredElemTypes],
        ): StagedCoproductInstances[F, T] = new StagedCoproductInstances[F, T] {
          def instances: List[Any] = s.instances

          def typetestsE(value: E[T])(implicit r: Reflection): List[E[Boolean]] =
            x.instances.map { case tpe: Type[_] => '{ $value.isInstanceOf[$tpe] } }

          def castsE(value: E[T])(implicit r: Reflection): List[E[Any]] = {
            x.instances.map { case tpe: Type[_] => '{ $value.asInstanceOf[$tpe] } }
          }
        }
    }
  }

  object K1 {
    type Id[t] = t
    type Const[c] = [t] =>> c

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
        assert(s.instances.nonEmpty)
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
      def accessorsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Any]]
      def constructorE[A](fields: List[E[Any]])(implicit r: Reflection): E[T[A]]

      def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]] = {
        val args = instances.safeZip(accessorsE(x)).map((in, a) => f(in.asInstanceOf, a.asInstanceOf))
        constructorE(args)
      }
    }

    object StagedProductInstances {
      implicit def apply[F[_[_]], T[_]]
        (implicit
          m: ProductGeneric[T],
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes]
        ): StagedProductInstances[F, T] = new StagedProductInstances[F, T] {
          def instances: List[Any] = s.instances

          def accessorsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Any]] = {
            import r._
            t.unseal.symbol match {
              case IsClassDefSymbol(self) =>
                self.caseFields.map { field =>
                  Select.unique(value.unseal, field.name).seal
                }
              case NoSymbol() =>
                t.unseal.tpe match {
                  case Type.TypeLambda(_, _, Type.AppliedType(tcons, _)) =>
                    tcons.classSymbol match {
                      case Some(IsClassDefSymbol(self)) =>
                        self.caseFields.map { field =>
                          Select.unique(value.unseal, field.name).seal
                        }
                      case _ => Nil
                    }
                  case _ => Nil
                }
            }
          }

          def constructorE[A](fields: List[E[Any]])(implicit r: Reflection): E[T[A]] = {
            import r._
            def companion(tpe: Type): TermRef = tpe match {
              case Type.SymRef(sym, prefix)   => Type.TermRef(prefix, sym.name)
              case Type.TypeRef(name, prefix) => Type.TermRef(prefix, name)
              case Type.TypeLambda(_, _, Type.AppliedType(tcons, _)) => companion(tcons)
            }
            def defaultTargs(tpe: Type): List[Type] = tpe match {
              case Type.SymRef(_, _)   => Nil
              case Type.TypeRef(_, _) => Nil
              case Type.TypeLambda(_, bounds, _) =>
                // Do we need to call .low for covariant cases? I don't think we do...
                bounds.map(_.hi)
            }
            val t0 = t.unseal.tpe
            Select.overloaded(Ident(companion(t0)), "apply", defaultTargs(t0), fields.map(_.unseal))
              .seal.asInstanceOf[E[T[A]]]
          }
        }
    }

    trait StagedCoproductInstances[F[_[_]], T[_]] extends StagedInstances[F, T] {
      def instances: List[Any]
      def typetestsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Boolean]]
      def castsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Any]]

      def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]] =
        instances.safeZip(typetestsE(x).safeZip(castsE(x))).foldLeft('{ null }) {
          case (acc, (i, (t, c))) => '{ if ($t) ${ f(i.asInstanceOf, c.asInstanceOf) } else $acc }
        }.asInstanceOf
    }

    object StagedCoproductInstances {
      implicit def apply[F[_[_]], T[_]]
        (implicit
          m: CoproductGeneric[T],
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes],
          x: SummonInstances[Type, m.MirroredElemTypes],
        ): StagedCoproductInstances[F, T] = new StagedCoproductInstances[F, T] {
          def instances: List[Any] = s.instances

          def typetestsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Boolean]] =
            x.instances.map { case tpe: Type[_] =>
              trait DummyK[X]
              val tpe0 = tpe.asInstanceOf[quoted.Type[DummyK]]
              '{ ${ value.asInstanceOf[E[T[Any]]] }.isInstanceOf[$tpe0[Any]] }
            }

          def castsE[A](value: E[T[A]])(implicit r: Reflection): List[E[Any]] =
            x.instances.map { case tpe: Type[_] =>
              import r._
              tpe.unseal.tpe match {
                case Type.TypeLambda(_, bound :: Nil, _) =>
                  trait DummyK[X]
                  val tpe0 = tpe.asInstanceOf[quoted.Type[DummyK]]
                  '{ ${ value.asInstanceOf[E[T[Any]]] }.asInstanceOf[$tpe0[Any]] }
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
      ${ eqStagedImpl('f1, 'f2)(e) }

    def eqStagedImpl[T](f1: E[T], f2: E[T])(e: Eq0[T])(implicit r: Reflection): E[Boolean] =
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

    def mapStagedImpl[F[_], A: Type, B: Type](fa: E[F[A]], f: E[A => B])(e: Functor0[F])(implicit r: Reflection): E[F[B]] =
      e.map(fa)(f)
  }

  trait Functor0[F[_]] {
    def map[A: Type, B: Type](fa: E[F[A]])(f: E[A => B]): RE[F[B]]
  }

  object Functor0 {
    import K1.{Id, Const}

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

// Benchmarks

type I = Int
type B = Boolean

case class P0()
case class P10(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B)
case class P20(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B)
case class P30(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B
)
case class P40(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B
)
case class P50(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B
)
case class P60(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B,
  a5: I, b5: B, c5: I, d5: B, e5: I, f5: B, g5: I, h5: B, i5: I, j5: B
)
case class P70(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B,
  a5: I, b5: B, c5: I, d5: B, e5: I, f5: B, g5: I, h5: B, i5: I, j5: B,
  a6: I, b6: B, c6: I, d6: B, e6: I, f6: B, g6: I, h6: B, i6: I, j6: B
)
case class P80(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B,
  a5: I, b5: B, c5: I, d5: B, e5: I, f5: B, g5: I, h5: B, i5: I, j5: B,
  a6: I, b6: B, c6: I, d6: B, e6: I, f6: B, g6: I, h6: B, i6: I, j6: B,
  a7: I, b7: B, c7: I, d7: B, e7: I, f7: B, g7: I, h7: B, i7: I, j7: B
)
case class P90(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B,
  a5: I, b5: B, c5: I, d5: B, e5: I, f5: B, g5: I, h5: B, i5: I, j5: B,
  a6: I, b6: B, c6: I, d6: B, e6: I, f6: B, g6: I, h6: B, i6: I, j6: B,
  a7: I, b7: B, c7: I, d7: B, e7: I, f7: B, g7: I, h7: B, i7: I, j7: B,
  a8: I, b8: B, c8: I, d8: B, e8: I, f8: B, g8: I, h8: B, i8: I, j8: B
)
case class P100(
  a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I, j0: B,
  a1: I, b1: B, c1: I, d1: B, e1: I, f1: B, g1: I, h1: B, i1: I, j1: B,
  a2: I, b2: B, c2: I, d2: B, e2: I, f2: B, g2: I, h2: B, i2: I, j2: B,
  a3: I, b3: B, c3: I, d3: B, e3: I, f3: B, g3: I, h3: B, i3: I, j3: B,
  a4: I, b4: B, c4: I, d4: B, e4: I, f4: B, g4: I, h4: B, i4: I, j4: B,
  a5: I, b5: B, c5: I, d5: B, e5: I, f5: B, g5: I, h5: B, i5: I, j5: B,
  a6: I, b6: B, c6: I, d6: B, e6: I, f6: B, g6: I, h6: B, i6: I, j6: B,
  a7: I, b7: B, c7: I, d7: B, e7: I, f7: B, g7: I, h7: B, i7: I, j7: B,
  a8: I, b8: B, c8: I, d8: B, e8: I, f8: B, g8: I, h8: B, i8: I, j8: B,
  a9: I, b9: B, c9: I, d9: B, e9: I, f9: B, g9: I, h9: B, i9: I, j9: B
)

sealed trait C0
enum C10 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B)
}
enum C20 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B)
}
enum C30 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B)
}
enum C40 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B)
}
enum C50 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B)
}
enum C60 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case C5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B)
}
enum C70 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case C5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case C6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B)
}
enum C80 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case C5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case C6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B);
  case A7(i: I); case B7(b: B); case C7(i: I); case D7(b: B); case E7(i: I); case F7(b: B); case G7(i: I); case H7(b: B); case I7(i: I); case J7(b: B)
}
enum C90 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case C5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case C6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B);
  case A7(i: I); case B7(b: B); case C7(i: I); case D7(b: B); case E7(i: I); case F7(b: B); case G7(i: I); case H7(b: B); case I7(i: I); case J7(b: B);
  case A8(i: I); case B8(b: B); case C8(i: I); case D8(b: B); case E8(i: I); case F8(b: B); case G8(i: I); case H8(b: B); case I8(i: I); case J8(b: B)
}
enum C100 {
  case A0(i: I); case B0(b: B); case C0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case C1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case C2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case C3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case C4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case C5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case C6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B);
  case A7(i: I); case B7(b: B); case C7(i: I); case D7(b: B); case E7(i: I); case F7(b: B); case G7(i: I); case H7(b: B); case I7(i: I); case J7(b: B);
  case A8(i: I); case B8(b: B); case C8(i: I); case D8(b: B); case E8(i: I); case F8(b: B); case G8(i: I); case H8(b: B); case I8(i: I); case J8(b: B);
  case A9(i: I); case B9(b: B); case C9(i: I); case D9(b: B); case E9(i: I); case F9(b: B); case G9(i: I); case H9(b: B); case I9(i: I); case J9(b: B)
}

case class PK0[A]()
case class PK10[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A
)
case class PK20[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A
)
case class PK30[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A
)
case class PK40[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A
)
case class PK50[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A
)
case class PK60[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A,
  a5: I, b5: A, c5: I, d5: A, e5: I, f5: A, g5: I, h5: A, i5: I, j5: A
)
case class PK70[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A,
  a5: I, b5: A, c5: I, d5: A, e5: I, f5: A, g5: I, h5: A, i5: I, j5: A,
  a6: I, b6: A, c6: I, d6: A, e6: I, f6: A, g6: I, h6: A, i6: I, j6: A
)
case class PK80[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A,
  a5: I, b5: A, c5: I, d5: A, e5: I, f5: A, g5: I, h5: A, i5: I, j5: A,
  a6: I, b6: A, c6: I, d6: A, e6: I, f6: A, g6: I, h6: A, i6: I, j6: A,
  a7: I, b7: A, c7: I, d7: A, e7: I, f7: A, g7: I, h7: A, i7: I, j7: A
)
case class PK90[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A,
  a5: I, b5: A, c5: I, d5: A, e5: I, f5: A, g5: I, h5: A, i5: I, j5: A,
  a6: I, b6: A, c6: I, d6: A, e6: I, f6: A, g6: I, h6: A, i6: I, j6: A,
  a7: I, b7: A, c7: I, d7: A, e7: I, f7: A, g7: I, h7: A, i7: I, j7: A,
  a8: I, b8: A, c8: I, d8: A, e8: I, f8: A, g8: I, h8: A, i8: I, j8: A
)
case class PK100[A](
  a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I, j0: A,
  a1: I, b1: A, c1: I, d1: A, e1: I, f1: A, g1: I, h1: A, i1: I, j1: A,
  a2: I, b2: A, c2: I, d2: A, e2: I, f2: A, g2: I, h2: A, i2: I, j2: A,
  a3: I, b3: A, c3: I, d3: A, e3: I, f3: A, g3: I, h3: A, i3: I, j3: A,
  a4: I, b4: A, c4: I, d4: A, e4: I, f4: A, g4: I, h4: A, i4: I, j4: A,
  a5: I, b5: A, c5: I, d5: A, e5: I, f5: A, g5: I, h5: A, i5: I, j5: A,
  a6: I, b6: A, c6: I, d6: A, e6: I, f6: A, g6: I, h6: A, i6: I, j6: A,
  a7: I, b7: A, c7: I, d7: A, e7: I, f7: A, g7: I, h7: A, i7: I, j7: A,
  a8: I, b8: A, c8: I, d8: A, e8: I, f8: A, g8: I, h8: A, i8: I, j8: A,
  a9: I, b9: A, c9: I, d9: A, e9: I, f9: A, g9: I, h9: A, i9: I, j9: A
)

sealed trait CK0[+A]
enum CK20[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B)
}
enum CK30[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B)
}
enum CK40[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B)
}
enum CK50[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B)
}
enum CK60[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case C5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B)
}
enum CK70[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case C5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case C6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B)
}
enum CK80[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case C5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case C6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B);
  case A7(a: A); case B7(b: B); case C7(a: A); case D7(b: B); case E7(a: A); case F7(b: B); case G7(a: A); case H7(b: B); case I7(a: A); case J7(b: B)
}
enum CK90[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case C5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case C6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B);
  case A7(a: A); case B7(b: B); case C7(a: A); case D7(b: B); case E7(a: A); case F7(b: B); case G7(a: A); case H7(b: B); case I7(a: A); case J7(b: B);
  case A8(a: A); case B8(b: B); case C8(a: A); case D8(b: B); case E8(a: A); case F8(b: B); case G8(a: A); case H8(b: B); case I8(a: A); case J8(b: B)
}
enum CK100[+A] {
  case A0(a: A); case B0(b: B); case C0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case C1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case C2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case C3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case C4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case C5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case C6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B);
  case A7(a: A); case B7(b: B); case C7(a: A); case D7(b: B); case E7(a: A); case F7(b: B); case G7(a: A); case H7(b: B); case I7(a: A); case J7(b: B);
  case A8(a: A); case B8(b: B); case C8(a: A); case D8(b: B); case E8(a: A); case F8(b: B); case G8(a: A); case H8(b: B); case I8(a: A); case J8(b: B);
  case A9(a: A); case B9(b: B); case C9(a: A); case D9(b: B); case E9(a: A); case F9(b: B); case G9(a: A); case H9(b: B); case I9(a: A); case J9(b: B)
}
