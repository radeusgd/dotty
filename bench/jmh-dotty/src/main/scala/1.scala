package scala

import scala.tasty._
import scala.deriving._
import scala.quoted.{Expr => E, _}
import scala.annotation.tailrec
import scala.compiletime._

object Shapeless3 {
  type Id[t] = t
  type Const[c] = [t] =>> c
  case class Wrap[T](t: T)

  type ~>[A[_], B[_]] = [t] => A[t] => B[t]

  inline def summon[T] = implicit match {
    case t: T => t
  }

  inline def summonAsArray[T <: Tuple]: Array[Any] =
    summonAsArray0[T](0, new Array[Any](constValue[Tuple.Size[T]]))

  inline def summonAsArray0[T](i: Int, arr: Array[Any]): Array[Any] = inline erasedValue[T] match {
    case _: Unit => arr
    case _: (a *: b) =>
      arr(i) = summon[a]
      summonAsArray0[b](i+1, arr)
  }

  sealed trait CompleteOr[T]
  case class Complete[T](t: T) extends CompleteOr[T]
  case class Continue[T](t: T) extends CompleteOr[T]

  object Complete {
    inline def apply[T](c: Boolean)(t: T)(f: T): CompleteOr[T] =
      if(c) Complete(t)
      else Continue(f)
  }

  abstract class ErasedInstances[FT] {
    def erasedMap(x: Any)(f: (Any, Any) => Any): Any
  }

  abstract class ErasedProductInstances[FT] extends ErasedInstances[FT] {
    def erasedConstruct(f: Any => Any): Any
    def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any])
    def erasedMap(x0: Any)(f: (Any, Any) => Any): Any
    def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any
    def erasedFoldLeft(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any
    def erasedFoldLeft2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any
  }

  final class ErasedProductInstances0[FT](val mirror: Mirror.Product) extends ErasedProductInstances[FT] {
    def erasedConstruct(f: Any => Any): Any = mirror.fromProduct(None)
    def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = (a, Some(mirror.fromProduct(None)))
    def erasedMap(x0: Any)(f: (Any, Any) => Any): Any = x0
    def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any = x0
    def erasedFoldLeft(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = a
    def erasedFoldLeft2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = a
  }

  final class ErasedProductInstances1[FT](val mirror: Mirror.Product, mkI: => Any) extends ErasedProductInstances[FT] {
    lazy val i = mkI

    inline def toProduct(x: Any): Product = x.asInstanceOf[Product]

    def erasedConstruct(f: Any => Any): Any =
      mirror.fromProduct(Tuple1(f(i)))

    def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = {
      val (acc0, e0) = f(a, i)
      e0 match {
        case Some(_) => (acc0, Some(mirror.fromProduct(e0)))
        case None => (acc0, None)
      }
    }

    def erasedMap(x0: Any)(f: (Any, Any) => Any): Any =
      mirror.fromProduct(Tuple1(f(i, toProduct(x0).productElement(0))))

    def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any =
      mirror.fromProduct(Tuple1(f(i, toProduct(x0).productElement(0), toProduct(y0).productElement(0))))

    def erasedFoldLeft(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
      f(a, i, toProduct(x0).productElement(0)) match {
        case Complete(r) => r
        case Continue(acc) => acc
      }
    }

    def erasedFoldLeft2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
      f(a, i, toProduct(x0).productElement(0), toProduct(y0).productElement(0)) match {
        case Complete(r) => r
        case Continue(acc) => acc
      }
    }
  }

  final class ErasedProductInstancesN[FT](val mirror: Mirror.Product, mkIs: => Array[Any]) extends ErasedProductInstances[FT] {
    import ErasedProductInstances.ArrayProduct

    lazy val is = mkIs

    inline def toProduct(x: Any): Product = x.asInstanceOf[Product]

    def erasedConstruct(f: Any => Any): Any = {
      val n = is.length
      val arr = new Array[Any](n)
      var i = 0
      while(i < n) {
        arr(i) = f(is(i))
        i = i+1
      }
      mirror.fromProduct(ArrayProduct(arr))
    }

    def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = {
      val n = is.length
      val arr = new Array[Any](n)
      var acc = a
      var i = 0
      while(i < n) {
        val (acc0, e0) = f(acc, is(i))
        e0 match {
          case Some(e) =>
            acc = acc0
            arr(i) = e
          case None =>
            return (acc0, None)
        }
        i = i+1
      }
      (acc, Some(mirror.fromProduct(ArrayProduct(arr))))
    }

    def erasedMap(x0: Any)(f: (Any, Any) => Any): Any = {
      val x = toProduct(x0)
      val n = is.length
      val arr = new Array[Any](n)
      var i = 0
      while(i < n) {
        arr(i) = f(is(i), x.productElement(i))
        i = i+1
      }
      mirror.fromProduct(ArrayProduct(arr))
    }

    def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any = {
      val x = toProduct(x0)
      val y = toProduct(y0)
      val n = is.length
      val arr = new Array[Any](n)
      var i = 0
      while(i < n) {
        arr(i) = f(is(i), x.productElement(i), y.productElement(i))
        i = i+1
      }
      mirror.fromProduct(ArrayProduct(arr))
    }

    def erasedFoldLeft(x0: Any)(i: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
      val x = toProduct(x0)
      val n = x.productArity
      @tailrec
      def loop(i: Int, acc: Any): Any =
        if(i >= n) acc
        else
          f(acc, is(i), x.productElement(i)) match {
            case Complete(r) => r
            case Continue(acc) =>
              loop(i+1, acc)
          }

      loop(0, i)
    }

    def erasedFoldLeft2(x0: Any, y0: Any)(i: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
      val x = toProduct(x0)
      val y = toProduct(y0)
      val n = x.productArity
      @tailrec
      def loop(i: Int, acc: Any): Any =
        if(i >= n) acc
        else
          f(acc, is(i), x.productElement(i), y.productElement(i)) match {
            case Complete(r) => r
            case Continue(acc) =>
              loop(i+1, acc)
          }

      loop(0, i)
    }
  }

  object ErasedProductInstances {
    class ArrayProduct(val elems: Array[Any]) extends Product {
      def canEqual(that: Any): Boolean = true
      def productElement(n: Int) = elems(n)
      def productArity = elems.length
      override def productIterator: Iterator[Any] = elems.iterator
    }

    inline def summonOne[T] = inline erasedValue[T] match {
      case _: Tuple1[a] => summon[a]
    }

    inline def apply[FT, E <: Tuple](mirror: Mirror.Product) : ErasedProductInstances[FT] =
      inline erasedValue[Tuple.Size[E]] match {
        case 0 => new ErasedProductInstances0[FT](mirror)
        case 1 => new ErasedProductInstances1[FT](mirror, summonOne[E])
        case _ => new ErasedProductInstancesN[FT](mirror, summonAsArray[E])
      }
  }

  final class ErasedCoproductInstances[FT](mirror: Mirror.Sum, mkIs: => Array[Any]) extends ErasedInstances[FT] {
    lazy val is = mkIs

    def ordinal(x: Any): Any = is(mirror.ordinal(x.asInstanceOf))

    def erasedMap(x: Any)(f: (Any, Any) => Any): Any = {
      val i = ordinal(x)
      f(i, x)
    }

    def erasedProject(p: Int)(i: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) =
      f(i, is(p))

    def erasedFold(x: Any)(f: (Any, Any) => Any): Any = {
      val i = ordinal(x)
      f(i, x)
    }

    def erasedFold2(x: Any, y: Any)(a: => Any)(f: (Any, Any, Any) => Any): Any = {
      val i = mirror.ordinal(x.asInstanceOf)
      val j = mirror.ordinal(y.asInstanceOf)
      if(i == j) f(is(i), x, y)
      else a
    }
  }

  object ErasedCoproductInstances {
    inline def apply[FT, E <: Tuple](mirror: Mirror.Sum) : ErasedCoproductInstances[FT] =
      new ErasedCoproductInstances[FT](mirror, summonAsArray[E])
  }

  object K0 {
    type Generic[O] = Mirror { type MirroredType = O ; type MirroredElemTypes }
    type ProductGeneric[O] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes }
    type CoproductGeneric[O] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes }

    def Generic[O] given (gen: Generic[O]): Generic[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen
    def ProductGeneric[O] given (gen: ProductGeneric[O]): ProductGeneric[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen
    def CoproductGeneric[O] given (gen: CoproductGeneric[O]): CoproductGeneric[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen

    type Instances[F[_], T] = ErasedInstances[F[T]]
    type ProductInstances[F[_], T] = ErasedProductInstances[F[T]]
    type CoproductInstances[F[_], T] = ErasedCoproductInstances[F[T]]

    def Instances[F[_], T] given (inst: Instances[F, T]): inst.type = inst
    def ProductInstances[F[_], T] given (inst: ProductInstances[F, T]): inst.type = inst
    def CoproductInstances[F[_], T] given (inst: CoproductInstances[F, T]): inst.type = inst

    type ToUnion[T] = T match {
      case Unit => Nothing
      case a *: b => a | ToUnion[b]
    }

    type IndexOf[E, X] = IndexOf0[E, X, 0]

    type IndexOf0[E, X, I <: Int] <: Int = X match {
      case Unit => -1
      case x *: xs => x match {
        case E => I
        case _ => IndexOf0[E, xs, S[I]]
      }
    }

    type LiftP[F[_], T] <: Tuple = T match {
      case Unit => Unit
      case a *: b => F[a] *: LiftP[F, b]
    }

    inline def summonFirst[F[_], T, U]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

    inline def summonFirst0[T] <: Any = inline erasedValue[T] match {
      case _: (a *: b) => implicit match {
        case aa: `a` => aa
        case _ => summonFirst0[b]
      }
    }

    given Ops {
      inline def (gen: ProductGeneric[Obj]) toRepr [Obj] (o: Obj): gen.MirroredElemTypes = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes]
      inline def (gen: ProductGeneric[Obj]) fromRepr [Obj] (r: gen.MirroredElemTypes): Obj = gen.fromProduct(r.asInstanceOf).asInstanceOf[Obj]

      inline def (gen: CoproductGeneric[Obj]) toRepr [Obj] (o: Obj): ToUnion[gen.MirroredElemTypes] = o.asInstanceOf
      inline def (gen: CoproductGeneric[Obj]) fromRepr [Obj] (r: ToUnion[gen.MirroredElemTypes]): Obj = r.asInstanceOf

      inline def (inst: Instances[F, T]) map [F[_], T] (x: T)(f: [t] => (F[t], t) => t): T =
        inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

      inline def (inst: ProductInstances[F, T]) construct [F[_], T] (f: [t] => F[t] => t): T =
        inst.erasedConstruct(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) unfold [F[_], T, Acc] (i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
        inst.erasedUnfold(i)(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) map2 [F[_], T] (x: T, y: T)(f: [t] => (F[t], t, t) => t): T =
        inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) foldLeft [F[_], T, Acc] (x: T)(i: Acc)(f: [t] => (Acc, F[t], t) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_], T, Acc] (x: T, y: T)(i: Acc)(f: [t] => (Acc, F[t], t, t) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

      inline def (inst: CoproductInstances[F, T]) project [F[_], T, Acc] (p: Int)(i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
        inst.erasedProject(p)(i)(f.asInstanceOf).asInstanceOf
      inline def (inst: CoproductInstances[F, T]) fold [F[_], T, R] (x: T)(f: [t] => (F[t], t) => R): R =
        inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
      inline def (inst: CoproductInstances[F, T]) fold2 [F[_], T, R] (x: T, y: T)(a: => R)(f: [t] => (F[t], t, t) => R): R =
        inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    }

    type ProductGenericR[O, R] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes = R }
    type CoproductGenericR[O, R] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes = R }

    inline given mkInstances[F[_], T] as ErasedInstances[F[T]] given (gen: Generic[T]) =
      inline gen match {
        case p: ProductGeneric[T]   => mkProductInstances[F, T] given p
        case c: CoproductGeneric[T] => mkCoproductInstances[F, T] given c
      }

    inline given mkProductInstances[F[_], T] as ErasedProductInstances[F[T]] given (gen: ProductGeneric[T]) =
      ErasedProductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

    inline given mkCoproductInstances[F[_], T] as ErasedCoproductInstances[F[T]] given (gen: CoproductGeneric[T]) =
      ErasedCoproductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)
  }

  object K1 {
    type Generic[O[_]] = Mirror { type MirroredType = O ; type MirroredElemTypes[_] }
    type ProductGeneric[O[_]] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes[_] }
    type CoproductGeneric[O[_]] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes[_] }

    def Generic[O[_]] given (gen: Generic[O]): gen.type = gen
    def ProductGeneric[O[_]] given (gen: ProductGeneric[O]): gen.type = gen
    def CoproductGeneric[O[_]] given (gen: CoproductGeneric[O]): gen.type = gen

    type Instances[F[_[_]], T[_]] = ErasedInstances[F[T]]
    type ProductInstances[F[_[_]], T[_]] = ErasedProductInstances[F[T]]
    type CoproductInstances[F[_[_]], T[_]] = ErasedCoproductInstances[F[T]]

    def Instances[F[_[_]], T[_]] given (inst: Instances[F, T]): inst.type = inst
    def ProductInstances[F[_[_]], T[_]] given (inst: ProductInstances[F, T]): inst.type = inst
    def CoproductInstances[F[_[_]], T[_]] given (inst: CoproductInstances[F, T]): inst.type = inst

    class Dummy
    type Apply[T[_]] = T[Dummy]
    type Unapply[F[_[_]], T] = T match {
      case Wrap[Apply[a]] => F[a]
      case Wrap[Dummy] => F[Id]
      case Wrap[c] => F[Const[c]]
    }

    type LiftP[F[_[_]], T[_]] = LiftP0[F, Apply[T]]

    type LiftP0[F[_[_]], T] <: Tuple = T match {
      case Unit => Unit
      case (a *:  b) => Unapply[F, Wrap[a]] *: LiftP0[F, b]
    }

    inline def summonFirst[F[_[_]], T[_], U[_]]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

    inline def summonFirst0[T] <: Any = inline erasedValue[T] match {
      case _: (a *: b) => implicit match {
        case aa: `a` => aa
        case _ => summonFirst0[b]
      }
    }

    given Ops {
      inline def (gen: ProductGeneric[Obj]) toRepr [Obj[_], A] (o: Obj[A]): gen.MirroredElemTypes[A] = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]
      inline def (gen: ProductGeneric[Obj]) fromRepr [Obj[_], A] (r: gen.MirroredElemTypes[A]): Obj[A] = gen.fromProduct(r.asInstanceOf).asInstanceOf[Obj[A]]

      inline def (gen: CoproductGeneric[Obj]) toRepr [Obj[_], A] (o: Obj[A]): K0.ToUnion[gen.MirroredElemTypes[A]] = o.asInstanceOf
      inline def (gen: CoproductGeneric[Obj]) fromRepr [Obj[_], A] (r: K0.ToUnion[gen.MirroredElemTypes[A]]): Obj[A] = r.asInstanceOf

      inline def (inst: Instances[F, T]) map[F[_[_]], T[_], A, R](x: T[A])(f: [t[_]] => (F[t], t[A]) => t[R]): T[R] =
        inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

      inline def (inst: ProductInstances[F, T]) construct [F[_[_]], T[_], R] (f: [t[_]] => F[t] => t[R]): T[R] =
        inst.erasedConstruct(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) map2 [F[_[_]], T[_], A, B, R] (x: T[A], y: T[B])(f: [t[_]] => (F[t], t[A], t[B]) => t[R]): T[R] =
        inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) foldLeft [F[_[_]], T[_], A, Acc] (x: T[A])(i: Acc)(f: [t[_]] => (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
      inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_[_]], T[_], A, B, Acc] (x: T[A], y: T[B])(i: Acc)(f: [t[_]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

      inline def (inst: CoproductInstances[F, T]) fold [F[_[_]], T[_], A, R] (x: T[A])(f: [t[_]] => (F[t], t[A]) => R): R =
        inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
      inline def (inst: CoproductInstances[F, T]) fold2 [F[_[_]], T[_], A, B, R] (x: T[A], y: T[B])(a: => R)(f: [t[_]] => (F[t], t[A], t[B]) => R): R =
        inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    }

    inline given mkInstances[F[_[_]], T[_]] as ErasedInstances[F[T]] given (gen: Generic[T]) =
      inline gen match {
        case p: ProductGeneric[T] => mkProductInstances[F, T] given p
        case c: CoproductGeneric[T] => mkCoproductInstances[F, T] given c
      }

    inline given mkProductInstances[F[_[_]], T[_]] as ErasedProductInstances[F[T]] given (gen: ProductGeneric[T]) =
      ErasedProductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

    inline given mkCoproductInstances[F[_[_]], T[_]] as ErasedCoproductInstances[F[T]] given (gen: CoproductGeneric[T]) =
      ErasedCoproductInstances[F[T], LiftP[F, gen.MirroredElemTypes]](gen)

    type LiftProductGeneric[O, E] = ProductGeneric[Const[O]] { type MirroredElemTypes = Const[E] }

    given mkK1_0[O] as LiftProductGeneric[O, k0.MirroredElemTypes] given (k0: K0.ProductGeneric[O]) = k0.asInstanceOf
  }

  trait Eq[A] {
    def eqv(x: A, y: A): Boolean
  }

  object Eq {
    inline def apply[A] given (ea: Eq[A]): Eq[A] = ea

    given as Eq[Unit] {
      def eqv(x: Unit, y: Unit): Boolean = true
    }
    given as Eq[Boolean] {
      def eqv(x: Boolean, y: Boolean): Boolean = x == y
    }
    given as Eq[Int] {
      def eqv(x: Int, y: Int): Boolean = x == y
    }
    given as Eq[String] {
      def eqv(x: String, y: String): Boolean = x == y
    }

    import K0.Ops._

    given eqGen[A] as Eq[A] given (inst: K0.ProductInstances[Eq, A]) {
      def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean)(
        [t] => (acc: Boolean, eqt: Eq[t], t0: t, t1: t) => Complete(!eqt.eqv(t0, t1))(false)(true)
      )
    }

    given eqGenC[A] as Eq[A] given (inst: => K0.CoproductInstances[Eq, A]) {
      def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false)(
        [t] => (eqt: Eq[t], t0: t, t1: t) => eqt.eqv(t0, t1)
      )
    }

    inline def derived[A] given (gen: K0.Generic[A]): Eq[A] = inline gen match {
      case p: K0.ProductGeneric[A]   => eqGen  given (K0.mkProductInstances given p)
      case c: K0.CoproductGeneric[A] => eqGenC given (K0.mkCoproductInstances given c)
    }
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    inline def apply[F[_]] given (ff: Functor[F]): Functor[F] = ff

    given as Functor[Id] {
      def map[A, B](a: A)(f: A => B): B = f(a)
    }

    given [F[_], G[_]] as Functor[[t] =>> F[G[t]]] given (ff: Functor[F], fg: Functor[G]) {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = ff.map(fga)(ga => fg.map(ga)(f))
    }

    import K1.Ops._

    given functorGen[F[_]] as Functor[F] given (inst: => K1.Instances[Functor, F]) {
      def map[A, B](fa: F[A])(f: A => B): F[B] = inst.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) => ft.map(ta)(f))
    }

    given [T] as Functor[Const[T]] {
      def map[A, B](t: T)(f: A => B): T = t
    }

    inline def derived[F[_]] given (gen: K1.Generic[F]): Functor[F] =
      functorGen given (K1.mkInstances given gen)
  }
}

object Inlined {
  trait Eq[T] {
    def eqv(x: T, y: T): Boolean
  }

  object Eq {
    import scala.compiletime.erasedValue
    import compiletime._
    import scala.deriving._

    inline def tryEql[TT](x: TT, y: TT): Boolean = delegate match {
      case eq: Eq[TT] => eq.eqv(x, y)
    }

    inline def eqlElemv[Elems <: Tuple](n: Int)(x: Any, y: Any): Boolean =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          tryEql[elem](productElement[elem](x, n), productElement[elem](y, n)) &&
          eqlElemv[elems1](n + 1)(x, y)
        case _: Unit =>
          true
      }

    inline def eqlProducv[T](m: Mirror.ProductOf[T])(x: Any, y: Any): Boolean =
      eqlElemv[m.MirroredElemTypes](0)(x, y)

    inline def eqlCasev[Alts](n: Int)(x: Any, y: Any, ord: Int): Boolean =
      inline erasedValue[Alts] match {
        case _: (alt *: alts1) =>
          if (ord == n)
            delegate match {
              case m: Mirror.ProductOf[`alt`] => eqlElemv[m.MirroredElemTypes](0)(x, y)
            }
          else eqlCasev[alts1](n + 1)(x, y, ord)
        case _: Unit =>
          false
      }

    inline def derived[T](implicit ev: Mirror.Of[T]): Eq[T] = new Eq[T] {
      def eqv(x: T, y: T): Boolean =
        inline ev match {
          case m: Mirror.SumOf[T] =>
            val ord = m.ordinal(x)
            ord == m.ordinal(y) && eqlCasev[m.MirroredElemTypes](0)(x, y, ord)
          case m: Mirror.ProductOf[T] =>
            eqlElemv[m.MirroredElemTypes](0)(x, y)
        }
    }

    implicit object IntEq extends Eq[Int] {
      def eqv(x: Int, y: Int) = x == y
    }

    implicit object BooleanEq extends Eq[Boolean] {
      def eqv(x: Boolean, y: Boolean) = x == y
    }
  }
}

object Staged {
  type RE[X] = given QuoteContext => E[X]
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

    trait StagedInstances[F[_], T]
    // { def map(x: T)(f: [t] => (F[t], t) => t): T }

    trait StagedProductInstances[F[_], T] extends StagedInstances[F, T] {
      implicit val tag: Type[T]
      def instances: List[Any]
      def accessorsE(value: E[T])(implicit q: QuoteContext): List[E[Any]]
      def constructorE(fields: List[E[Any]])(implicit q: QuoteContext): E[T]

      def foldLeft2E[Acc: Type](x0: E[T], y0: E[T])(i: E[Acc])(f: [t] => (E[Acc], F[t], E[t], E[t]) => E[Acc]): RE[Acc] = {
        def re(x: E[T], y: E[T]) =
          accessorsE(x).safeZip(accessorsE(y)).safeZip(instances).foldLeft(i) {
            case (acc, ((xn, yn), in)) =>
              f(acc, in.asInstanceOf, xn, yn)
          }
        // val instanceOrRecurse = null
        '{
          def foo(x: T, y: T) =
            ${ re('x, 'y) }
          foo($x0, $y0)
        }
         //   def merge(a: E[T], b: E[T]): E[Acc] = {
         //     $re
         //   }
         //   ${ merge(x, y) }
         // }
      }
    }

    object StagedProductInstances {
      implicit def apply[F[_], T]
        given erased (m: Mirror.ProductOf[T])
        given (
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes]
        ): StagedProductInstances[F, T] = new StagedProductInstances[F, T] {
          val tag = t

          def instances: List[Any] = s.instances

          def accessorsE(value: E[T])(implicit q: QuoteContext): List[E[Any]] = {
            import q.tasty._
            t.unseal.symbol match {
              case IsClassDefSymbol(self) => // case class
                self.caseFields.map { field =>
                  Select.unique(value.unseal, field.name).seal
                }
              case _ =>
                Nil // case object
            }
          }

          def constructorE(fields: List[E[Any]])(implicit q: QuoteContext): E[T] = {
            import q.tasty._
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
        given erased (m: Mirror.SumOf[T])
        given (
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
        given erased (m: ProductGeneric[T])
        given (
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes]
        ): StagedProductInstances[F, T] = new StagedProductInstances[F, T] {
          def instances: List[Any] = s.instances

          def accessorsE[A](value: E[T[A]])(implicit q: QuoteContext): List[E[Any]] = {
            import q.tasty._
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

          def constructorE[A](fields: List[E[Any]])(implicit q: QuoteContext): E[T[A]] = {
            import q.tasty._
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
      def typetestsE[A](value: E[T[A]])(implicit q: QuoteContext): List[E[Boolean]]
      def castsE[A](value: E[T[A]])(implicit q: QuoteContext): List[E[Any]]

      def mapE[A, R](x: E[T[A]])(f: [t[_]] => (F[t], E[t[A]]) => E[t[R]]): RE[T[R]] =
        instances.safeZip(typetestsE(x).safeZip(castsE(x))).foldLeft('{ null }) {
          case (acc, (i, (t, c))) => '{ if ($t) ${ f(i.asInstanceOf, c.asInstanceOf) } else $acc }
        }.asInstanceOf
    }

    object StagedCoproductInstances {
      implicit def apply[F[_[_]], T[_]]
        given erased (m: CoproductGeneric[T])
        given (
          t: Type[T],
          s: SummonInstances[F, m.MirroredElemTypes],
          x: SummonInstances[Type, m.MirroredElemTypes],
        ): StagedCoproductInstances[F, T] = new StagedCoproductInstances[F, T] {
          def instances: List[Any] = s.instances

          def typetestsE[A](value: E[T[A]])(implicit q: QuoteContext): List[E[Boolean]] =
            x.instances.map { case tpe: Type[_] =>
              // This is a hack to compensate for a shortcoming of type
              // patterns with AnyKind. Here we *know* that x.instances is
              // composed of `Type[Fn[_]]` for n = 1..x.size, so we should
              // like to write `case tpe: Type[_[_]]`, but that's outside of
              // the current syntax. Thankfully all we need is to splice that
              // `tpe` to perform a type test, which we manage to do in a very
              // forceful way using a dummy trait with the desired kind:
              trait DummyK[X]
              val tpe0 = tpe.asInstanceOf[quoted.Type[DummyK]]
              '{ ${ value.asInstanceOf[E[T[Any]]] }.isInstanceOf[$tpe0[Any]] }
            }

          def castsE[A](value: E[T[A]])(implicit q: QuoteContext): List[E[Any]] =
            x.instances.map { case tpe: Type[_] =>
            import q.tasty._
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

// Benchmarks
/*
type I = Int
type B = Boolean

case class P0()
case class P1(a0: I)
case class P2(a0: I, b0: B)
case class P3(a0: I, b0: B, c0: I)
case class P4(a0: I, b0: B, c0: I, d0: B)
case class P5(a0: I, b0: B, c0: I, d0: B, e0: I)
case class P6(a0: I, b0: B, c0: I, d0: B, e0: I, f0: B)
case class P7(a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I)
case class P8(a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B)
case class P9(a0: I, b0: B, c0: I, d0: B, e0: I, f0: B, g0: I, h0: B, i0: I)
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

enum C1 { case A0(i: I) }
enum C2 { case A0(i: I); case B0(b: B) }
enum C3 { case A0(i: I); case B0(b: B); case S0(i: I) }
enum C4 { case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B) }
enum C5 { case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I) }
enum C6 { case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B) }
enum C7 { case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I) }
enum C8 { case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B) }
enum C9 { case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I) }
enum C10 {
  case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
}
enum C20 {
  case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case S1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
}
enum C30 {
  case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case S1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case S2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
}
enum C40 {
  case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case S1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case S2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case S3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
}
enum C50 {
  case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case S1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case S2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case S3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case S4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
}
enum C60 {
  case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case S1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case S2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case S3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case S4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case S5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
}
enum C70 {
  case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case S1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case S2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case S3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case S4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case S5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case S6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B);
}
enum C80 {
  case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case S1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case S2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case S3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case S4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case S5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case S6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B);
  case A7(i: I); case B7(b: B); case S7(i: I); case D7(b: B); case E7(i: I); case F7(b: B); case G7(i: I); case H7(b: B); case I7(i: I); case J7(b: B);
}
enum C90 {
  case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case S1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case S2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case S3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case S4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case S5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case S6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B);
  case A7(i: I); case B7(b: B); case S7(i: I); case D7(b: B); case E7(i: I); case F7(b: B); case G7(i: I); case H7(b: B); case I7(i: I); case J7(b: B);
  case A8(i: I); case B8(b: B); case S8(i: I); case D8(b: B); case E8(i: I); case F8(b: B); case G8(i: I); case H8(b: B); case I8(i: I); case J8(b: B);
}
enum C100 {
  case A0(i: I); case B0(b: B); case S0(i: I); case D0(b: B); case E0(i: I); case F0(b: B); case G0(i: I); case H0(b: B); case I0(i: I); case J0(b: B);
  case A1(i: I); case B1(b: B); case S1(i: I); case D1(b: B); case E1(i: I); case F1(b: B); case G1(i: I); case H1(b: B); case I1(i: I); case J1(b: B);
  case A2(i: I); case B2(b: B); case S2(i: I); case D2(b: B); case E2(i: I); case F2(b: B); case G2(i: I); case H2(b: B); case I2(i: I); case J2(b: B);
  case A3(i: I); case B3(b: B); case S3(i: I); case D3(b: B); case E3(i: I); case F3(b: B); case G3(i: I); case H3(b: B); case I3(i: I); case J3(b: B);
  case A4(i: I); case B4(b: B); case S4(i: I); case D4(b: B); case E4(i: I); case F4(b: B); case G4(i: I); case H4(b: B); case I4(i: I); case J4(b: B);
  case A5(i: I); case B5(b: B); case S5(i: I); case D5(b: B); case E5(i: I); case F5(b: B); case G5(i: I); case H5(b: B); case I5(i: I); case J5(b: B);
  case A6(i: I); case B6(b: B); case S6(i: I); case D6(b: B); case E6(i: I); case F6(b: B); case G6(i: I); case H6(b: B); case I6(i: I); case J6(b: B);
  case A7(i: I); case B7(b: B); case S7(i: I); case D7(b: B); case E7(i: I); case F7(b: B); case G7(i: I); case H7(b: B); case I7(i: I); case J7(b: B);
  case A8(i: I); case B8(b: B); case S8(i: I); case D8(b: B); case E8(i: I); case F8(b: B); case G8(i: I); case H8(b: B); case I8(i: I); case J8(b: B);
  case A9(i: I); case B9(b: B); case S9(i: I); case D9(b: B); case E9(i: I); case F9(b: B); case G9(i: I); case H9(b: B); case I9(i: I); case J9(b: B);
}

case class PK0[A]()
case class PK1[A](a0: I)
case class PK2[A](a0: I, b0: A)
case class PK3[A](a0: I, b0: A, c0: I)
case class PK4[A](a0: I, b0: A, c0: I, d0: A)
case class PK5[A](a0: I, b0: A, c0: I, d0: A, e0: I)
case class PK6[A](a0: I, b0: A, c0: I, d0: A, e0: I, f0: A)
case class PK7[A](a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I)
case class PK8[A](a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A)
case class PK9[A](a0: I, b0: A, c0: I, d0: A, e0: I, f0: A, g0: I, h0: A, i0: I)
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

enum CK1[+A] { case A0(a: A) }
enum CK2[+A] { case A0(a: A); case B0(b: B) }
enum CK3[+A] { case A0(a: A); case B0(b: B); case S0(a: A) }
enum CK4[+A] { case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B) }
enum CK5[+A] { case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A) }
enum CK6[+A] { case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B) }
enum CK7[+A] { case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A) }
enum CK8[+A] { case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B) }
enum CK9[+A] { case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A) }
enum CK10[+A] {
  case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
}
enum CK20[+A] {
  case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case S1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
}
enum CK30[+A] {
  case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case S1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case S2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
}
enum CK40[+A] {
  case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case S1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case S2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case S3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
}
enum CK50[+A] {
  case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case S1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case S2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case S3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case S4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
}
enum CK60[+A] {
  case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case S1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case S2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case S3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case S4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case S5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
}
enum CK70[+A] {
  case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case S1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case S2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case S3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case S4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case S5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case S6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B);
}
enum CK80[+A] {
  case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case S1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case S2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case S3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case S4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case S5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case S6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B);
  case A7(a: A); case B7(b: B); case S7(a: A); case D7(b: B); case E7(a: A); case F7(b: B); case G7(a: A); case H7(b: B); case I7(a: A); case J7(b: B);
}
enum CK90[+A] {
  case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case S1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case S2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case S3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case S4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case S5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case S6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B);
  case A7(a: A); case B7(b: B); case S7(a: A); case D7(b: B); case E7(a: A); case F7(b: B); case G7(a: A); case H7(b: B); case I7(a: A); case J7(b: B);
  case A8(a: A); case B8(b: B); case S8(a: A); case D8(b: B); case E8(a: A); case F8(b: B); case G8(a: A); case H8(b: B); case I8(a: A); case J8(b: B);
}
enum CK100[+A] {
  case A0(a: A); case B0(b: B); case S0(a: A); case D0(b: B); case E0(a: A); case F0(b: B); case G0(a: A); case H0(b: B); case I0(a: A); case J0(b: B);
  case A1(a: A); case B1(b: B); case S1(a: A); case D1(b: B); case E1(a: A); case F1(b: B); case G1(a: A); case H1(b: B); case I1(a: A); case J1(b: B);
  case A2(a: A); case B2(b: B); case S2(a: A); case D2(b: B); case E2(a: A); case F2(b: B); case G2(a: A); case H2(b: B); case I2(a: A); case J2(b: B);
  case A3(a: A); case B3(b: B); case S3(a: A); case D3(b: B); case E3(a: A); case F3(b: B); case G3(a: A); case H3(b: B); case I3(a: A); case J3(b: B);
  case A4(a: A); case B4(b: B); case S4(a: A); case D4(b: B); case E4(a: A); case F4(b: B); case G4(a: A); case H4(b: B); case I4(a: A); case J4(b: B);
  case A5(a: A); case B5(b: B); case S5(a: A); case D5(b: B); case E5(a: A); case F5(b: B); case G5(a: A); case H5(b: B); case I5(a: A); case J5(b: B);
  case A6(a: A); case B6(b: B); case S6(a: A); case D6(b: B); case E6(a: A); case F6(b: B); case G6(a: A); case H6(b: B); case I6(a: A); case J6(b: B);
  case A7(a: A); case B7(b: B); case S7(a: A); case D7(b: B); case E7(a: A); case F7(b: B); case G7(a: A); case H7(b: B); case I7(a: A); case J7(b: B);
  case A8(a: A); case B8(b: B); case S8(a: A); case D8(b: B); case E8(a: A); case F8(b: B); case G8(a: A); case H8(b: B); case I8(a: A); case J8(b: B);
  case A9(a: A); case B9(b: B); case S9(a: A); case D9(b: B); case E9(a: A); case F9(b: B); case G9(a: A); case H9(b: B); case I9(a: A); case J9(b: B);
}

object Hand {
  trait Eq[A] {
    def eqv(x: A, y: A): Boolean
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object EqP0 extends Eq[P0] {
    def eqv(x: P0, y: P0): Boolean = true
  } // EqP0
  object EqP1 extends Eq[P1] {
    def eqv(x: P1, y: P1): Boolean = {
      x.a0 == y.a0
    }
  } // EqP1
  object EqP2 extends Eq[P2] {
    def eqv(x: P2, y: P2): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0
    }
  } // EqP2
  object EqP3 extends Eq[P3] {
    def eqv(x: P3, y: P3): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0
    }
  } // EqP3
  object EqP4 extends Eq[P4] {
    def eqv(x: P4, y: P4): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0
    }
  } // EqP4
  object EqP5 extends Eq[P5] {
    def eqv(x: P5, y: P5): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0
    }
  } // EqP5
  object EqP6 extends Eq[P6] {
    def eqv(x: P6, y: P6): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0
    }
  } // EqP6
  object EqP7 extends Eq[P7] {
    def eqv(x: P7, y: P7): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0
    }
  } // EqP7
  object EqP8 extends Eq[P8] {
    def eqv(x: P8, y: P8): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0
    }
  } // EqP8
  object EqP9 extends Eq[P9] {
    def eqv(x: P9, y: P9): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0
    }
  } // EqP9

  object EqP10 extends Eq[P10] {
    def eqv(x: P10, y: P10): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0
    }
  } // EqP10
  object EqP20 extends Eq[P20] {
    def eqv(x: P20, y: P20): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1
    }
  } // EqP20
  object EqP30 extends Eq[P30] {
    def eqv(x: P30, y: P30): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1 &&
      x.a2 == y.a2 && x.b2 == y.b2 && x.c2 == y.c2 && x.d2 == y.d2 && x.e2 == y.e2 && x.f2 == y.f2 && x.g2 == y.g2 && x.h2 == y.h2 && x.i2 == y.i2 && x.j2 == y.j2
    }
  } // EqP30
  object EqP40 extends Eq[P40] {
    def eqv(x: P40, y: P40): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1 &&
      x.a2 == y.a2 && x.b2 == y.b2 && x.c2 == y.c2 && x.d2 == y.d2 && x.e2 == y.e2 && x.f2 == y.f2 && x.g2 == y.g2 && x.h2 == y.h2 && x.i2 == y.i2 && x.j2 == y.j2 &&
      x.a3 == y.a3 && x.b3 == y.b3 && x.c3 == y.c3 && x.d3 == y.d3 && x.e3 == y.e3 && x.f3 == y.f3 && x.g3 == y.g3 && x.h3 == y.h3 && x.i3 == y.i3 && x.j3 == y.j3
    }
  } // EqP40
  object EqP50 extends Eq[P50] {
    def eqv(x: P50, y: P50): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1 &&
      x.a2 == y.a2 && x.b2 == y.b2 && x.c2 == y.c2 && x.d2 == y.d2 && x.e2 == y.e2 && x.f2 == y.f2 && x.g2 == y.g2 && x.h2 == y.h2 && x.i2 == y.i2 && x.j2 == y.j2 &&
      x.a3 == y.a3 && x.b3 == y.b3 && x.c3 == y.c3 && x.d3 == y.d3 && x.e3 == y.e3 && x.f3 == y.f3 && x.g3 == y.g3 && x.h3 == y.h3 && x.i3 == y.i3 && x.j3 == y.j3 &&
      x.a4 == y.a4 && x.b4 == y.b4 && x.c4 == y.c4 && x.d4 == y.d4 && x.e4 == y.e4 && x.f4 == y.f4 && x.g4 == y.g4 && x.h4 == y.h4 && x.i4 == y.i4 && x.j4 == y.j4
    }
  } // EqP50
  object EqP60 extends Eq[P60] {
    def eqv(x: P60, y: P60): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1 &&
      x.a2 == y.a2 && x.b2 == y.b2 && x.c2 == y.c2 && x.d2 == y.d2 && x.e2 == y.e2 && x.f2 == y.f2 && x.g2 == y.g2 && x.h2 == y.h2 && x.i2 == y.i2 && x.j2 == y.j2 &&
      x.a3 == y.a3 && x.b3 == y.b3 && x.c3 == y.c3 && x.d3 == y.d3 && x.e3 == y.e3 && x.f3 == y.f3 && x.g3 == y.g3 && x.h3 == y.h3 && x.i3 == y.i3 && x.j3 == y.j3 &&
      x.a4 == y.a4 && x.b4 == y.b4 && x.c4 == y.c4 && x.d4 == y.d4 && x.e4 == y.e4 && x.f4 == y.f4 && x.g4 == y.g4 && x.h4 == y.h4 && x.i4 == y.i4 && x.j4 == y.j4 &&
      x.a5 == y.a5 && x.b5 == y.b5 && x.c5 == y.c5 && x.d5 == y.d5 && x.e5 == y.e5 && x.f5 == y.f5 && x.g5 == y.g5 && x.h5 == y.h5 && x.i5 == y.i5 && x.j5 == y.j5
    }
  } // EqP60
  object EqP70 extends Eq[P70] {
    def eqv(x: P70, y: P70): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1 &&
      x.a2 == y.a2 && x.b2 == y.b2 && x.c2 == y.c2 && x.d2 == y.d2 && x.e2 == y.e2 && x.f2 == y.f2 && x.g2 == y.g2 && x.h2 == y.h2 && x.i2 == y.i2 && x.j2 == y.j2 &&
      x.a3 == y.a3 && x.b3 == y.b3 && x.c3 == y.c3 && x.d3 == y.d3 && x.e3 == y.e3 && x.f3 == y.f3 && x.g3 == y.g3 && x.h3 == y.h3 && x.i3 == y.i3 && x.j3 == y.j3 &&
      x.a4 == y.a4 && x.b4 == y.b4 && x.c4 == y.c4 && x.d4 == y.d4 && x.e4 == y.e4 && x.f4 == y.f4 && x.g4 == y.g4 && x.h4 == y.h4 && x.i4 == y.i4 && x.j4 == y.j4 &&
      x.a5 == y.a5 && x.b5 == y.b5 && x.c5 == y.c5 && x.d5 == y.d5 && x.e5 == y.e5 && x.f5 == y.f5 && x.g5 == y.g5 && x.h5 == y.h5 && x.i5 == y.i5 && x.j5 == y.j5 &&
      x.a6 == y.a6 && x.b6 == y.b6 && x.c6 == y.c6 && x.d6 == y.d6 && x.e6 == y.e6 && x.f6 == y.f6 && x.g6 == y.g6 && x.h6 == y.h6 && x.i6 == y.i6 && x.j6 == y.j6
    }
  } // EqP70
  object EqP80 extends Eq[P80] {
    def eqv(x: P80, y: P80): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1 &&
      x.a2 == y.a2 && x.b2 == y.b2 && x.c2 == y.c2 && x.d2 == y.d2 && x.e2 == y.e2 && x.f2 == y.f2 && x.g2 == y.g2 && x.h2 == y.h2 && x.i2 == y.i2 && x.j2 == y.j2 &&
      x.a3 == y.a3 && x.b3 == y.b3 && x.c3 == y.c3 && x.d3 == y.d3 && x.e3 == y.e3 && x.f3 == y.f3 && x.g3 == y.g3 && x.h3 == y.h3 && x.i3 == y.i3 && x.j3 == y.j3 &&
      x.a4 == y.a4 && x.b4 == y.b4 && x.c4 == y.c4 && x.d4 == y.d4 && x.e4 == y.e4 && x.f4 == y.f4 && x.g4 == y.g4 && x.h4 == y.h4 && x.i4 == y.i4 && x.j4 == y.j4 &&
      x.a5 == y.a5 && x.b5 == y.b5 && x.c5 == y.c5 && x.d5 == y.d5 && x.e5 == y.e5 && x.f5 == y.f5 && x.g5 == y.g5 && x.h5 == y.h5 && x.i5 == y.i5 && x.j5 == y.j5 &&
      x.a6 == y.a6 && x.b6 == y.b6 && x.c6 == y.c6 && x.d6 == y.d6 && x.e6 == y.e6 && x.f6 == y.f6 && x.g6 == y.g6 && x.h6 == y.h6 && x.i6 == y.i6 && x.j6 == y.j6 &&
      x.a7 == y.a7 && x.b7 == y.b7 && x.c7 == y.c7 && x.d7 == y.d7 && x.e7 == y.e7 && x.f7 == y.f7 && x.g7 == y.g7 && x.h7 == y.h7 && x.i7 == y.i7 && x.j7 == y.j7
    }
  } // EqP80
  object EqP90 extends Eq[P90] {
    def eqv(x: P90, y: P90): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1 &&
      x.a2 == y.a2 && x.b2 == y.b2 && x.c2 == y.c2 && x.d2 == y.d2 && x.e2 == y.e2 && x.f2 == y.f2 && x.g2 == y.g2 && x.h2 == y.h2 && x.i2 == y.i2 && x.j2 == y.j2 &&
      x.a3 == y.a3 && x.b3 == y.b3 && x.c3 == y.c3 && x.d3 == y.d3 && x.e3 == y.e3 && x.f3 == y.f3 && x.g3 == y.g3 && x.h3 == y.h3 && x.i3 == y.i3 && x.j3 == y.j3 &&
      x.a4 == y.a4 && x.b4 == y.b4 && x.c4 == y.c4 && x.d4 == y.d4 && x.e4 == y.e4 && x.f4 == y.f4 && x.g4 == y.g4 && x.h4 == y.h4 && x.i4 == y.i4 && x.j4 == y.j4 &&
      x.a5 == y.a5 && x.b5 == y.b5 && x.c5 == y.c5 && x.d5 == y.d5 && x.e5 == y.e5 && x.f5 == y.f5 && x.g5 == y.g5 && x.h5 == y.h5 && x.i5 == y.i5 && x.j5 == y.j5 &&
      x.a6 == y.a6 && x.b6 == y.b6 && x.c6 == y.c6 && x.d6 == y.d6 && x.e6 == y.e6 && x.f6 == y.f6 && x.g6 == y.g6 && x.h6 == y.h6 && x.i6 == y.i6 && x.j6 == y.j6 &&
      x.a7 == y.a7 && x.b7 == y.b7 && x.c7 == y.c7 && x.d7 == y.d7 && x.e7 == y.e7 && x.f7 == y.f7 && x.g7 == y.g7 && x.h7 == y.h7 && x.i7 == y.i7 && x.j7 == y.j7 &&
      x.a8 == y.a8 && x.b8 == y.b8 && x.c8 == y.c8 && x.d8 == y.d8 && x.e8 == y.e8 && x.f8 == y.f8 && x.g8 == y.g8 && x.h8 == y.h8 && x.i8 == y.i8 && x.j8 == y.j8
    }
  } // EqP90
  object EqP100 extends Eq[P100] {
    def eqv(x: P100, y: P100): Boolean = {
      x.a0 == y.a0 && x.b0 == y.b0 && x.c0 == y.c0 && x.d0 == y.d0 && x.e0 == y.e0 && x.f0 == y.f0 && x.g0 == y.g0 && x.h0 == y.h0 && x.i0 == y.i0 && x.j0 == y.j0 &&
      x.a1 == y.a1 && x.b1 == y.b1 && x.c1 == y.c1 && x.d1 == y.d1 && x.e1 == y.e1 && x.f1 == y.f1 && x.g1 == y.g1 && x.h1 == y.h1 && x.i1 == y.i1 && x.j1 == y.j1 &&
      x.a2 == y.a2 && x.b2 == y.b2 && x.c2 == y.c2 && x.d2 == y.d2 && x.e2 == y.e2 && x.f2 == y.f2 && x.g2 == y.g2 && x.h2 == y.h2 && x.i2 == y.i2 && x.j2 == y.j2 &&
      x.a3 == y.a3 && x.b3 == y.b3 && x.c3 == y.c3 && x.d3 == y.d3 && x.e3 == y.e3 && x.f3 == y.f3 && x.g3 == y.g3 && x.h3 == y.h3 && x.i3 == y.i3 && x.j3 == y.j3 &&
      x.a4 == y.a4 && x.b4 == y.b4 && x.c4 == y.c4 && x.d4 == y.d4 && x.e4 == y.e4 && x.f4 == y.f4 && x.g4 == y.g4 && x.h4 == y.h4 && x.i4 == y.i4 && x.j4 == y.j4 &&
      x.a5 == y.a5 && x.b5 == y.b5 && x.c5 == y.c5 && x.d5 == y.d5 && x.e5 == y.e5 && x.f5 == y.f5 && x.g5 == y.g5 && x.h5 == y.h5 && x.i5 == y.i5 && x.j5 == y.j5 &&
      x.a6 == y.a6 && x.b6 == y.b6 && x.c6 == y.c6 && x.d6 == y.d6 && x.e6 == y.e6 && x.f6 == y.f6 && x.g6 == y.g6 && x.h6 == y.h6 && x.i6 == y.i6 && x.j6 == y.j6 &&
      x.a7 == y.a7 && x.b7 == y.b7 && x.c7 == y.c7 && x.d7 == y.d7 && x.e7 == y.e7 && x.f7 == y.f7 && x.g7 == y.g7 && x.h7 == y.h7 && x.i7 == y.i7 && x.j7 == y.j7 &&
      x.a8 == y.a8 && x.b8 == y.b8 && x.c8 == y.c8 && x.d8 == y.d8 && x.e8 == y.e8 && x.f8 == y.f8 && x.g8 == y.g8 && x.h8 == y.h8 && x.i8 == y.i8 && x.j8 == y.j8 &&
      x.a9 == y.a9 && x.b9 == y.b9 && x.c9 == y.c9 && x.d9 == y.d9 && x.e9 == y.e9 && x.f9 == y.f9 && x.g9 == y.g9 && x.h9 == y.h9 && x.i9 == y.i9 && x.j9 == y.j9
    }
  } // EqP100

  object EqC1 extends Eq[C1] {
    import C1._
    def eqv(x: C1, y: C1): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z
    }
  } // EqC1
  object EqC2 extends Eq[C2] {
    import C2._
    def eqv(x: C2, y: C2): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z
    }
  } // EqC2
  object EqC3 extends Eq[C3] {
    import C3._
    def eqv(x: C3, y: C3): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z
    }
  } // EqC3
  object EqC4 extends Eq[C4] {
    import C4._
    def eqv(x: C4, y: C4): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z
    }
  } // EqC4
  object EqC5 extends Eq[C5] {
    import C5._
    def eqv(x: C5, y: C5): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z
    }
  } // EqC5
  object EqC6 extends Eq[C6] {
    import C6._
    def eqv(x: C6, y: C6): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z
    }
  } // EqC6
  object EqC7 extends Eq[C7] {
    import C7._
    def eqv(x: C7, y: C7): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z
    }
  } // EqC7
  object EqC8 extends Eq[C8] {
    import C8._
    def eqv(x: C8, y: C8): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z
    }
  } // EqC8
  object EqC9 extends Eq[C9] {
    import C9._
    def eqv(x: C9, y: C9): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z
    }
  } // EqC9
  object EqC10 extends Eq[C10] {
    import C10._
    def eqv(x: C10, y: C10): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
    }
  } // EqC10
  object EqC20 extends Eq[C20] {
    import C20._
    def eqv(x: C20, y: C20): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
      case A1(z) => y.isInstanceOf[A1] && y.asInstanceOf[A1].i == z; case B1(z) => y.isInstanceOf[B1] && y.asInstanceOf[B1].b == z; case S1(z) => y.isInstanceOf[S1] && y.asInstanceOf[S1].i == z; case D1(z) => y.isInstanceOf[D1] && y.asInstanceOf[D1].b == z; case E1(z) => y.isInstanceOf[E1] && y.asInstanceOf[E1].i == z; case F1(z) => y.isInstanceOf[F1] && y.asInstanceOf[F1].b == z; case G1(z) => y.isInstanceOf[G1] && y.asInstanceOf[G1].i == z; case H1(z) => y.isInstanceOf[H1] && y.asInstanceOf[H1].b == z; case I1(z) => y.isInstanceOf[I1] && y.asInstanceOf[I1].i == z; case J1(z) => y.isInstanceOf[J1] && y.asInstanceOf[J1].b == z
    }
  } // EqC20
  object EqC30 extends Eq[C30] {
    import C30._
    def eqv(x: C30, y: C30): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
      case A1(z) => y.isInstanceOf[A1] && y.asInstanceOf[A1].i == z; case B1(z) => y.isInstanceOf[B1] && y.asInstanceOf[B1].b == z; case S1(z) => y.isInstanceOf[S1] && y.asInstanceOf[S1].i == z; case D1(z) => y.isInstanceOf[D1] && y.asInstanceOf[D1].b == z; case E1(z) => y.isInstanceOf[E1] && y.asInstanceOf[E1].i == z; case F1(z) => y.isInstanceOf[F1] && y.asInstanceOf[F1].b == z; case G1(z) => y.isInstanceOf[G1] && y.asInstanceOf[G1].i == z; case H1(z) => y.isInstanceOf[H1] && y.asInstanceOf[H1].b == z; case I1(z) => y.isInstanceOf[I1] && y.asInstanceOf[I1].i == z; case J1(z) => y.isInstanceOf[J1] && y.asInstanceOf[J1].b == z
      case A2(z) => y.isInstanceOf[A2] && y.asInstanceOf[A2].i == z; case B2(z) => y.isInstanceOf[B2] && y.asInstanceOf[B2].b == z; case S2(z) => y.isInstanceOf[S2] && y.asInstanceOf[S2].i == z; case D2(z) => y.isInstanceOf[D2] && y.asInstanceOf[D2].b == z; case E2(z) => y.isInstanceOf[E2] && y.asInstanceOf[E2].i == z; case F2(z) => y.isInstanceOf[F2] && y.asInstanceOf[F2].b == z; case G2(z) => y.isInstanceOf[G2] && y.asInstanceOf[G2].i == z; case H2(z) => y.isInstanceOf[H2] && y.asInstanceOf[H2].b == z; case I2(z) => y.isInstanceOf[I2] && y.asInstanceOf[I2].i == z; case J2(z) => y.isInstanceOf[J2] && y.asInstanceOf[J2].b == z
    }
  } // EqC30
  object EqC40 extends Eq[C40] {
    import C40._
    def eqv(x: C40, y: C40): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
      case A1(z) => y.isInstanceOf[A1] && y.asInstanceOf[A1].i == z; case B1(z) => y.isInstanceOf[B1] && y.asInstanceOf[B1].b == z; case S1(z) => y.isInstanceOf[S1] && y.asInstanceOf[S1].i == z; case D1(z) => y.isInstanceOf[D1] && y.asInstanceOf[D1].b == z; case E1(z) => y.isInstanceOf[E1] && y.asInstanceOf[E1].i == z; case F1(z) => y.isInstanceOf[F1] && y.asInstanceOf[F1].b == z; case G1(z) => y.isInstanceOf[G1] && y.asInstanceOf[G1].i == z; case H1(z) => y.isInstanceOf[H1] && y.asInstanceOf[H1].b == z; case I1(z) => y.isInstanceOf[I1] && y.asInstanceOf[I1].i == z; case J1(z) => y.isInstanceOf[J1] && y.asInstanceOf[J1].b == z
      case A2(z) => y.isInstanceOf[A2] && y.asInstanceOf[A2].i == z; case B2(z) => y.isInstanceOf[B2] && y.asInstanceOf[B2].b == z; case S2(z) => y.isInstanceOf[S2] && y.asInstanceOf[S2].i == z; case D2(z) => y.isInstanceOf[D2] && y.asInstanceOf[D2].b == z; case E2(z) => y.isInstanceOf[E2] && y.asInstanceOf[E2].i == z; case F2(z) => y.isInstanceOf[F2] && y.asInstanceOf[F2].b == z; case G2(z) => y.isInstanceOf[G2] && y.asInstanceOf[G2].i == z; case H2(z) => y.isInstanceOf[H2] && y.asInstanceOf[H2].b == z; case I2(z) => y.isInstanceOf[I2] && y.asInstanceOf[I2].i == z; case J2(z) => y.isInstanceOf[J2] && y.asInstanceOf[J2].b == z
      case A3(z) => y.isInstanceOf[A3] && y.asInstanceOf[A3].i == z; case B3(z) => y.isInstanceOf[B3] && y.asInstanceOf[B3].b == z; case S3(z) => y.isInstanceOf[S3] && y.asInstanceOf[S3].i == z; case D3(z) => y.isInstanceOf[D3] && y.asInstanceOf[D3].b == z; case E3(z) => y.isInstanceOf[E3] && y.asInstanceOf[E3].i == z; case F3(z) => y.isInstanceOf[F3] && y.asInstanceOf[F3].b == z; case G3(z) => y.isInstanceOf[G3] && y.asInstanceOf[G3].i == z; case H3(z) => y.isInstanceOf[H3] && y.asInstanceOf[H3].b == z; case I3(z) => y.isInstanceOf[I3] && y.asInstanceOf[I3].i == z; case J3(z) => y.isInstanceOf[J3] && y.asInstanceOf[J3].b == z
    }
  } // EqC40
  object EqC50 extends Eq[C50] {
    import C50._
    def eqv(x: C50, y: C50): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
      case A1(z) => y.isInstanceOf[A1] && y.asInstanceOf[A1].i == z; case B1(z) => y.isInstanceOf[B1] && y.asInstanceOf[B1].b == z; case S1(z) => y.isInstanceOf[S1] && y.asInstanceOf[S1].i == z; case D1(z) => y.isInstanceOf[D1] && y.asInstanceOf[D1].b == z; case E1(z) => y.isInstanceOf[E1] && y.asInstanceOf[E1].i == z; case F1(z) => y.isInstanceOf[F1] && y.asInstanceOf[F1].b == z; case G1(z) => y.isInstanceOf[G1] && y.asInstanceOf[G1].i == z; case H1(z) => y.isInstanceOf[H1] && y.asInstanceOf[H1].b == z; case I1(z) => y.isInstanceOf[I1] && y.asInstanceOf[I1].i == z; case J1(z) => y.isInstanceOf[J1] && y.asInstanceOf[J1].b == z
      case A2(z) => y.isInstanceOf[A2] && y.asInstanceOf[A2].i == z; case B2(z) => y.isInstanceOf[B2] && y.asInstanceOf[B2].b == z; case S2(z) => y.isInstanceOf[S2] && y.asInstanceOf[S2].i == z; case D2(z) => y.isInstanceOf[D2] && y.asInstanceOf[D2].b == z; case E2(z) => y.isInstanceOf[E2] && y.asInstanceOf[E2].i == z; case F2(z) => y.isInstanceOf[F2] && y.asInstanceOf[F2].b == z; case G2(z) => y.isInstanceOf[G2] && y.asInstanceOf[G2].i == z; case H2(z) => y.isInstanceOf[H2] && y.asInstanceOf[H2].b == z; case I2(z) => y.isInstanceOf[I2] && y.asInstanceOf[I2].i == z; case J2(z) => y.isInstanceOf[J2] && y.asInstanceOf[J2].b == z
      case A3(z) => y.isInstanceOf[A3] && y.asInstanceOf[A3].i == z; case B3(z) => y.isInstanceOf[B3] && y.asInstanceOf[B3].b == z; case S3(z) => y.isInstanceOf[S3] && y.asInstanceOf[S3].i == z; case D3(z) => y.isInstanceOf[D3] && y.asInstanceOf[D3].b == z; case E3(z) => y.isInstanceOf[E3] && y.asInstanceOf[E3].i == z; case F3(z) => y.isInstanceOf[F3] && y.asInstanceOf[F3].b == z; case G3(z) => y.isInstanceOf[G3] && y.asInstanceOf[G3].i == z; case H3(z) => y.isInstanceOf[H3] && y.asInstanceOf[H3].b == z; case I3(z) => y.isInstanceOf[I3] && y.asInstanceOf[I3].i == z; case J3(z) => y.isInstanceOf[J3] && y.asInstanceOf[J3].b == z
      case A4(z) => y.isInstanceOf[A4] && y.asInstanceOf[A4].i == z; case B4(z) => y.isInstanceOf[B4] && y.asInstanceOf[B4].b == z; case S4(z) => y.isInstanceOf[S4] && y.asInstanceOf[S4].i == z; case D4(z) => y.isInstanceOf[D4] && y.asInstanceOf[D4].b == z; case E4(z) => y.isInstanceOf[E4] && y.asInstanceOf[E4].i == z; case F4(z) => y.isInstanceOf[F4] && y.asInstanceOf[F4].b == z; case G4(z) => y.isInstanceOf[G4] && y.asInstanceOf[G4].i == z; case H4(z) => y.isInstanceOf[H4] && y.asInstanceOf[H4].b == z; case I4(z) => y.isInstanceOf[I4] && y.asInstanceOf[I4].i == z; case J4(z) => y.isInstanceOf[J4] && y.asInstanceOf[J4].b == z
    }
  } // EqC50
  object EqC60 extends Eq[C60] {
    import C60._
    def eqv(x: C60, y: C60): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
      case A1(z) => y.isInstanceOf[A1] && y.asInstanceOf[A1].i == z; case B1(z) => y.isInstanceOf[B1] && y.asInstanceOf[B1].b == z; case S1(z) => y.isInstanceOf[S1] && y.asInstanceOf[S1].i == z; case D1(z) => y.isInstanceOf[D1] && y.asInstanceOf[D1].b == z; case E1(z) => y.isInstanceOf[E1] && y.asInstanceOf[E1].i == z; case F1(z) => y.isInstanceOf[F1] && y.asInstanceOf[F1].b == z; case G1(z) => y.isInstanceOf[G1] && y.asInstanceOf[G1].i == z; case H1(z) => y.isInstanceOf[H1] && y.asInstanceOf[H1].b == z; case I1(z) => y.isInstanceOf[I1] && y.asInstanceOf[I1].i == z; case J1(z) => y.isInstanceOf[J1] && y.asInstanceOf[J1].b == z
      case A2(z) => y.isInstanceOf[A2] && y.asInstanceOf[A2].i == z; case B2(z) => y.isInstanceOf[B2] && y.asInstanceOf[B2].b == z; case S2(z) => y.isInstanceOf[S2] && y.asInstanceOf[S2].i == z; case D2(z) => y.isInstanceOf[D2] && y.asInstanceOf[D2].b == z; case E2(z) => y.isInstanceOf[E2] && y.asInstanceOf[E2].i == z; case F2(z) => y.isInstanceOf[F2] && y.asInstanceOf[F2].b == z; case G2(z) => y.isInstanceOf[G2] && y.asInstanceOf[G2].i == z; case H2(z) => y.isInstanceOf[H2] && y.asInstanceOf[H2].b == z; case I2(z) => y.isInstanceOf[I2] && y.asInstanceOf[I2].i == z; case J2(z) => y.isInstanceOf[J2] && y.asInstanceOf[J2].b == z
      case A3(z) => y.isInstanceOf[A3] && y.asInstanceOf[A3].i == z; case B3(z) => y.isInstanceOf[B3] && y.asInstanceOf[B3].b == z; case S3(z) => y.isInstanceOf[S3] && y.asInstanceOf[S3].i == z; case D3(z) => y.isInstanceOf[D3] && y.asInstanceOf[D3].b == z; case E3(z) => y.isInstanceOf[E3] && y.asInstanceOf[E3].i == z; case F3(z) => y.isInstanceOf[F3] && y.asInstanceOf[F3].b == z; case G3(z) => y.isInstanceOf[G3] && y.asInstanceOf[G3].i == z; case H3(z) => y.isInstanceOf[H3] && y.asInstanceOf[H3].b == z; case I3(z) => y.isInstanceOf[I3] && y.asInstanceOf[I3].i == z; case J3(z) => y.isInstanceOf[J3] && y.asInstanceOf[J3].b == z
      case A4(z) => y.isInstanceOf[A4] && y.asInstanceOf[A4].i == z; case B4(z) => y.isInstanceOf[B4] && y.asInstanceOf[B4].b == z; case S4(z) => y.isInstanceOf[S4] && y.asInstanceOf[S4].i == z; case D4(z) => y.isInstanceOf[D4] && y.asInstanceOf[D4].b == z; case E4(z) => y.isInstanceOf[E4] && y.asInstanceOf[E4].i == z; case F4(z) => y.isInstanceOf[F4] && y.asInstanceOf[F4].b == z; case G4(z) => y.isInstanceOf[G4] && y.asInstanceOf[G4].i == z; case H4(z) => y.isInstanceOf[H4] && y.asInstanceOf[H4].b == z; case I4(z) => y.isInstanceOf[I4] && y.asInstanceOf[I4].i == z; case J4(z) => y.isInstanceOf[J4] && y.asInstanceOf[J4].b == z
      case A5(z) => y.isInstanceOf[A5] && y.asInstanceOf[A5].i == z; case B5(z) => y.isInstanceOf[B5] && y.asInstanceOf[B5].b == z; case S5(z) => y.isInstanceOf[S5] && y.asInstanceOf[S5].i == z; case D5(z) => y.isInstanceOf[D5] && y.asInstanceOf[D5].b == z; case E5(z) => y.isInstanceOf[E5] && y.asInstanceOf[E5].i == z; case F5(z) => y.isInstanceOf[F5] && y.asInstanceOf[F5].b == z; case G5(z) => y.isInstanceOf[G5] && y.asInstanceOf[G5].i == z; case H5(z) => y.isInstanceOf[H5] && y.asInstanceOf[H5].b == z; case I5(z) => y.isInstanceOf[I5] && y.asInstanceOf[I5].i == z; case J5(z) => y.isInstanceOf[J5] && y.asInstanceOf[J5].b == z
    }
  } // EqC60
  object EqC70 extends Eq[C70] {
    import C70._
    def eqv(x: C70, y: C70): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
      case A1(z) => y.isInstanceOf[A1] && y.asInstanceOf[A1].i == z; case B1(z) => y.isInstanceOf[B1] && y.asInstanceOf[B1].b == z; case S1(z) => y.isInstanceOf[S1] && y.asInstanceOf[S1].i == z; case D1(z) => y.isInstanceOf[D1] && y.asInstanceOf[D1].b == z; case E1(z) => y.isInstanceOf[E1] && y.asInstanceOf[E1].i == z; case F1(z) => y.isInstanceOf[F1] && y.asInstanceOf[F1].b == z; case G1(z) => y.isInstanceOf[G1] && y.asInstanceOf[G1].i == z; case H1(z) => y.isInstanceOf[H1] && y.asInstanceOf[H1].b == z; case I1(z) => y.isInstanceOf[I1] && y.asInstanceOf[I1].i == z; case J1(z) => y.isInstanceOf[J1] && y.asInstanceOf[J1].b == z
      case A2(z) => y.isInstanceOf[A2] && y.asInstanceOf[A2].i == z; case B2(z) => y.isInstanceOf[B2] && y.asInstanceOf[B2].b == z; case S2(z) => y.isInstanceOf[S2] && y.asInstanceOf[S2].i == z; case D2(z) => y.isInstanceOf[D2] && y.asInstanceOf[D2].b == z; case E2(z) => y.isInstanceOf[E2] && y.asInstanceOf[E2].i == z; case F2(z) => y.isInstanceOf[F2] && y.asInstanceOf[F2].b == z; case G2(z) => y.isInstanceOf[G2] && y.asInstanceOf[G2].i == z; case H2(z) => y.isInstanceOf[H2] && y.asInstanceOf[H2].b == z; case I2(z) => y.isInstanceOf[I2] && y.asInstanceOf[I2].i == z; case J2(z) => y.isInstanceOf[J2] && y.asInstanceOf[J2].b == z
      case A3(z) => y.isInstanceOf[A3] && y.asInstanceOf[A3].i == z; case B3(z) => y.isInstanceOf[B3] && y.asInstanceOf[B3].b == z; case S3(z) => y.isInstanceOf[S3] && y.asInstanceOf[S3].i == z; case D3(z) => y.isInstanceOf[D3] && y.asInstanceOf[D3].b == z; case E3(z) => y.isInstanceOf[E3] && y.asInstanceOf[E3].i == z; case F3(z) => y.isInstanceOf[F3] && y.asInstanceOf[F3].b == z; case G3(z) => y.isInstanceOf[G3] && y.asInstanceOf[G3].i == z; case H3(z) => y.isInstanceOf[H3] && y.asInstanceOf[H3].b == z; case I3(z) => y.isInstanceOf[I3] && y.asInstanceOf[I3].i == z; case J3(z) => y.isInstanceOf[J3] && y.asInstanceOf[J3].b == z
      case A4(z) => y.isInstanceOf[A4] && y.asInstanceOf[A4].i == z; case B4(z) => y.isInstanceOf[B4] && y.asInstanceOf[B4].b == z; case S4(z) => y.isInstanceOf[S4] && y.asInstanceOf[S4].i == z; case D4(z) => y.isInstanceOf[D4] && y.asInstanceOf[D4].b == z; case E4(z) => y.isInstanceOf[E4] && y.asInstanceOf[E4].i == z; case F4(z) => y.isInstanceOf[F4] && y.asInstanceOf[F4].b == z; case G4(z) => y.isInstanceOf[G4] && y.asInstanceOf[G4].i == z; case H4(z) => y.isInstanceOf[H4] && y.asInstanceOf[H4].b == z; case I4(z) => y.isInstanceOf[I4] && y.asInstanceOf[I4].i == z; case J4(z) => y.isInstanceOf[J4] && y.asInstanceOf[J4].b == z
      case A5(z) => y.isInstanceOf[A5] && y.asInstanceOf[A5].i == z; case B5(z) => y.isInstanceOf[B5] && y.asInstanceOf[B5].b == z; case S5(z) => y.isInstanceOf[S5] && y.asInstanceOf[S5].i == z; case D5(z) => y.isInstanceOf[D5] && y.asInstanceOf[D5].b == z; case E5(z) => y.isInstanceOf[E5] && y.asInstanceOf[E5].i == z; case F5(z) => y.isInstanceOf[F5] && y.asInstanceOf[F5].b == z; case G5(z) => y.isInstanceOf[G5] && y.asInstanceOf[G5].i == z; case H5(z) => y.isInstanceOf[H5] && y.asInstanceOf[H5].b == z; case I5(z) => y.isInstanceOf[I5] && y.asInstanceOf[I5].i == z; case J5(z) => y.isInstanceOf[J5] && y.asInstanceOf[J5].b == z
      case A6(z) => y.isInstanceOf[A6] && y.asInstanceOf[A6].i == z; case B6(z) => y.isInstanceOf[B6] && y.asInstanceOf[B6].b == z; case S6(z) => y.isInstanceOf[S6] && y.asInstanceOf[S6].i == z; case D6(z) => y.isInstanceOf[D6] && y.asInstanceOf[D6].b == z; case E6(z) => y.isInstanceOf[E6] && y.asInstanceOf[E6].i == z; case F6(z) => y.isInstanceOf[F6] && y.asInstanceOf[F6].b == z; case G6(z) => y.isInstanceOf[G6] && y.asInstanceOf[G6].i == z; case H6(z) => y.isInstanceOf[H6] && y.asInstanceOf[H6].b == z; case I6(z) => y.isInstanceOf[I6] && y.asInstanceOf[I6].i == z; case J6(z) => y.isInstanceOf[J6] && y.asInstanceOf[J6].b == z
    }
  } // EqC70
  object EqC80 extends Eq[C80] {
    import C80._
    def eqv(x: C80, y: C80): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
      case A1(z) => y.isInstanceOf[A1] && y.asInstanceOf[A1].i == z; case B1(z) => y.isInstanceOf[B1] && y.asInstanceOf[B1].b == z; case S1(z) => y.isInstanceOf[S1] && y.asInstanceOf[S1].i == z; case D1(z) => y.isInstanceOf[D1] && y.asInstanceOf[D1].b == z; case E1(z) => y.isInstanceOf[E1] && y.asInstanceOf[E1].i == z; case F1(z) => y.isInstanceOf[F1] && y.asInstanceOf[F1].b == z; case G1(z) => y.isInstanceOf[G1] && y.asInstanceOf[G1].i == z; case H1(z) => y.isInstanceOf[H1] && y.asInstanceOf[H1].b == z; case I1(z) => y.isInstanceOf[I1] && y.asInstanceOf[I1].i == z; case J1(z) => y.isInstanceOf[J1] && y.asInstanceOf[J1].b == z
      case A2(z) => y.isInstanceOf[A2] && y.asInstanceOf[A2].i == z; case B2(z) => y.isInstanceOf[B2] && y.asInstanceOf[B2].b == z; case S2(z) => y.isInstanceOf[S2] && y.asInstanceOf[S2].i == z; case D2(z) => y.isInstanceOf[D2] && y.asInstanceOf[D2].b == z; case E2(z) => y.isInstanceOf[E2] && y.asInstanceOf[E2].i == z; case F2(z) => y.isInstanceOf[F2] && y.asInstanceOf[F2].b == z; case G2(z) => y.isInstanceOf[G2] && y.asInstanceOf[G2].i == z; case H2(z) => y.isInstanceOf[H2] && y.asInstanceOf[H2].b == z; case I2(z) => y.isInstanceOf[I2] && y.asInstanceOf[I2].i == z; case J2(z) => y.isInstanceOf[J2] && y.asInstanceOf[J2].b == z
      case A3(z) => y.isInstanceOf[A3] && y.asInstanceOf[A3].i == z; case B3(z) => y.isInstanceOf[B3] && y.asInstanceOf[B3].b == z; case S3(z) => y.isInstanceOf[S3] && y.asInstanceOf[S3].i == z; case D3(z) => y.isInstanceOf[D3] && y.asInstanceOf[D3].b == z; case E3(z) => y.isInstanceOf[E3] && y.asInstanceOf[E3].i == z; case F3(z) => y.isInstanceOf[F3] && y.asInstanceOf[F3].b == z; case G3(z) => y.isInstanceOf[G3] && y.asInstanceOf[G3].i == z; case H3(z) => y.isInstanceOf[H3] && y.asInstanceOf[H3].b == z; case I3(z) => y.isInstanceOf[I3] && y.asInstanceOf[I3].i == z; case J3(z) => y.isInstanceOf[J3] && y.asInstanceOf[J3].b == z
      case A4(z) => y.isInstanceOf[A4] && y.asInstanceOf[A4].i == z; case B4(z) => y.isInstanceOf[B4] && y.asInstanceOf[B4].b == z; case S4(z) => y.isInstanceOf[S4] && y.asInstanceOf[S4].i == z; case D4(z) => y.isInstanceOf[D4] && y.asInstanceOf[D4].b == z; case E4(z) => y.isInstanceOf[E4] && y.asInstanceOf[E4].i == z; case F4(z) => y.isInstanceOf[F4] && y.asInstanceOf[F4].b == z; case G4(z) => y.isInstanceOf[G4] && y.asInstanceOf[G4].i == z; case H4(z) => y.isInstanceOf[H4] && y.asInstanceOf[H4].b == z; case I4(z) => y.isInstanceOf[I4] && y.asInstanceOf[I4].i == z; case J4(z) => y.isInstanceOf[J4] && y.asInstanceOf[J4].b == z
      case A5(z) => y.isInstanceOf[A5] && y.asInstanceOf[A5].i == z; case B5(z) => y.isInstanceOf[B5] && y.asInstanceOf[B5].b == z; case S5(z) => y.isInstanceOf[S5] && y.asInstanceOf[S5].i == z; case D5(z) => y.isInstanceOf[D5] && y.asInstanceOf[D5].b == z; case E5(z) => y.isInstanceOf[E5] && y.asInstanceOf[E5].i == z; case F5(z) => y.isInstanceOf[F5] && y.asInstanceOf[F5].b == z; case G5(z) => y.isInstanceOf[G5] && y.asInstanceOf[G5].i == z; case H5(z) => y.isInstanceOf[H5] && y.asInstanceOf[H5].b == z; case I5(z) => y.isInstanceOf[I5] && y.asInstanceOf[I5].i == z; case J5(z) => y.isInstanceOf[J5] && y.asInstanceOf[J5].b == z
      case A6(z) => y.isInstanceOf[A6] && y.asInstanceOf[A6].i == z; case B6(z) => y.isInstanceOf[B6] && y.asInstanceOf[B6].b == z; case S6(z) => y.isInstanceOf[S6] && y.asInstanceOf[S6].i == z; case D6(z) => y.isInstanceOf[D6] && y.asInstanceOf[D6].b == z; case E6(z) => y.isInstanceOf[E6] && y.asInstanceOf[E6].i == z; case F6(z) => y.isInstanceOf[F6] && y.asInstanceOf[F6].b == z; case G6(z) => y.isInstanceOf[G6] && y.asInstanceOf[G6].i == z; case H6(z) => y.isInstanceOf[H6] && y.asInstanceOf[H6].b == z; case I6(z) => y.isInstanceOf[I6] && y.asInstanceOf[I6].i == z; case J6(z) => y.isInstanceOf[J6] && y.asInstanceOf[J6].b == z
      case A7(z) => y.isInstanceOf[A7] && y.asInstanceOf[A7].i == z; case B7(z) => y.isInstanceOf[B7] && y.asInstanceOf[B7].b == z; case S7(z) => y.isInstanceOf[S7] && y.asInstanceOf[S7].i == z; case D7(z) => y.isInstanceOf[D7] && y.asInstanceOf[D7].b == z; case E7(z) => y.isInstanceOf[E7] && y.asInstanceOf[E7].i == z; case F7(z) => y.isInstanceOf[F7] && y.asInstanceOf[F7].b == z; case G7(z) => y.isInstanceOf[G7] && y.asInstanceOf[G7].i == z; case H7(z) => y.isInstanceOf[H7] && y.asInstanceOf[H7].b == z; case I7(z) => y.isInstanceOf[I7] && y.asInstanceOf[I7].i == z; case J7(z) => y.isInstanceOf[J7] && y.asInstanceOf[J7].b == z
    }
  } // EqC80
  object EqC90 extends Eq[C90] {
    import C90._
    def eqv(x: C90, y: C90): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
      case A1(z) => y.isInstanceOf[A1] && y.asInstanceOf[A1].i == z; case B1(z) => y.isInstanceOf[B1] && y.asInstanceOf[B1].b == z; case S1(z) => y.isInstanceOf[S1] && y.asInstanceOf[S1].i == z; case D1(z) => y.isInstanceOf[D1] && y.asInstanceOf[D1].b == z; case E1(z) => y.isInstanceOf[E1] && y.asInstanceOf[E1].i == z; case F1(z) => y.isInstanceOf[F1] && y.asInstanceOf[F1].b == z; case G1(z) => y.isInstanceOf[G1] && y.asInstanceOf[G1].i == z; case H1(z) => y.isInstanceOf[H1] && y.asInstanceOf[H1].b == z; case I1(z) => y.isInstanceOf[I1] && y.asInstanceOf[I1].i == z; case J1(z) => y.isInstanceOf[J1] && y.asInstanceOf[J1].b == z
      case A2(z) => y.isInstanceOf[A2] && y.asInstanceOf[A2].i == z; case B2(z) => y.isInstanceOf[B2] && y.asInstanceOf[B2].b == z; case S2(z) => y.isInstanceOf[S2] && y.asInstanceOf[S2].i == z; case D2(z) => y.isInstanceOf[D2] && y.asInstanceOf[D2].b == z; case E2(z) => y.isInstanceOf[E2] && y.asInstanceOf[E2].i == z; case F2(z) => y.isInstanceOf[F2] && y.asInstanceOf[F2].b == z; case G2(z) => y.isInstanceOf[G2] && y.asInstanceOf[G2].i == z; case H2(z) => y.isInstanceOf[H2] && y.asInstanceOf[H2].b == z; case I2(z) => y.isInstanceOf[I2] && y.asInstanceOf[I2].i == z; case J2(z) => y.isInstanceOf[J2] && y.asInstanceOf[J2].b == z
      case A3(z) => y.isInstanceOf[A3] && y.asInstanceOf[A3].i == z; case B3(z) => y.isInstanceOf[B3] && y.asInstanceOf[B3].b == z; case S3(z) => y.isInstanceOf[S3] && y.asInstanceOf[S3].i == z; case D3(z) => y.isInstanceOf[D3] && y.asInstanceOf[D3].b == z; case E3(z) => y.isInstanceOf[E3] && y.asInstanceOf[E3].i == z; case F3(z) => y.isInstanceOf[F3] && y.asInstanceOf[F3].b == z; case G3(z) => y.isInstanceOf[G3] && y.asInstanceOf[G3].i == z; case H3(z) => y.isInstanceOf[H3] && y.asInstanceOf[H3].b == z; case I3(z) => y.isInstanceOf[I3] && y.asInstanceOf[I3].i == z; case J3(z) => y.isInstanceOf[J3] && y.asInstanceOf[J3].b == z
      case A4(z) => y.isInstanceOf[A4] && y.asInstanceOf[A4].i == z; case B4(z) => y.isInstanceOf[B4] && y.asInstanceOf[B4].b == z; case S4(z) => y.isInstanceOf[S4] && y.asInstanceOf[S4].i == z; case D4(z) => y.isInstanceOf[D4] && y.asInstanceOf[D4].b == z; case E4(z) => y.isInstanceOf[E4] && y.asInstanceOf[E4].i == z; case F4(z) => y.isInstanceOf[F4] && y.asInstanceOf[F4].b == z; case G4(z) => y.isInstanceOf[G4] && y.asInstanceOf[G4].i == z; case H4(z) => y.isInstanceOf[H4] && y.asInstanceOf[H4].b == z; case I4(z) => y.isInstanceOf[I4] && y.asInstanceOf[I4].i == z; case J4(z) => y.isInstanceOf[J4] && y.asInstanceOf[J4].b == z
      case A5(z) => y.isInstanceOf[A5] && y.asInstanceOf[A5].i == z; case B5(z) => y.isInstanceOf[B5] && y.asInstanceOf[B5].b == z; case S5(z) => y.isInstanceOf[S5] && y.asInstanceOf[S5].i == z; case D5(z) => y.isInstanceOf[D5] && y.asInstanceOf[D5].b == z; case E5(z) => y.isInstanceOf[E5] && y.asInstanceOf[E5].i == z; case F5(z) => y.isInstanceOf[F5] && y.asInstanceOf[F5].b == z; case G5(z) => y.isInstanceOf[G5] && y.asInstanceOf[G5].i == z; case H5(z) => y.isInstanceOf[H5] && y.asInstanceOf[H5].b == z; case I5(z) => y.isInstanceOf[I5] && y.asInstanceOf[I5].i == z; case J5(z) => y.isInstanceOf[J5] && y.asInstanceOf[J5].b == z
      case A6(z) => y.isInstanceOf[A6] && y.asInstanceOf[A6].i == z; case B6(z) => y.isInstanceOf[B6] && y.asInstanceOf[B6].b == z; case S6(z) => y.isInstanceOf[S6] && y.asInstanceOf[S6].i == z; case D6(z) => y.isInstanceOf[D6] && y.asInstanceOf[D6].b == z; case E6(z) => y.isInstanceOf[E6] && y.asInstanceOf[E6].i == z; case F6(z) => y.isInstanceOf[F6] && y.asInstanceOf[F6].b == z; case G6(z) => y.isInstanceOf[G6] && y.asInstanceOf[G6].i == z; case H6(z) => y.isInstanceOf[H6] && y.asInstanceOf[H6].b == z; case I6(z) => y.isInstanceOf[I6] && y.asInstanceOf[I6].i == z; case J6(z) => y.isInstanceOf[J6] && y.asInstanceOf[J6].b == z
      case A7(z) => y.isInstanceOf[A7] && y.asInstanceOf[A7].i == z; case B7(z) => y.isInstanceOf[B7] && y.asInstanceOf[B7].b == z; case S7(z) => y.isInstanceOf[S7] && y.asInstanceOf[S7].i == z; case D7(z) => y.isInstanceOf[D7] && y.asInstanceOf[D7].b == z; case E7(z) => y.isInstanceOf[E7] && y.asInstanceOf[E7].i == z; case F7(z) => y.isInstanceOf[F7] && y.asInstanceOf[F7].b == z; case G7(z) => y.isInstanceOf[G7] && y.asInstanceOf[G7].i == z; case H7(z) => y.isInstanceOf[H7] && y.asInstanceOf[H7].b == z; case I7(z) => y.isInstanceOf[I7] && y.asInstanceOf[I7].i == z; case J7(z) => y.isInstanceOf[J7] && y.asInstanceOf[J7].b == z
      case A8(z) => y.isInstanceOf[A8] && y.asInstanceOf[A8].i == z; case B8(z) => y.isInstanceOf[B8] && y.asInstanceOf[B8].b == z; case S8(z) => y.isInstanceOf[S8] && y.asInstanceOf[S8].i == z; case D8(z) => y.isInstanceOf[D8] && y.asInstanceOf[D8].b == z; case E8(z) => y.isInstanceOf[E8] && y.asInstanceOf[E8].i == z; case F8(z) => y.isInstanceOf[F8] && y.asInstanceOf[F8].b == z; case G8(z) => y.isInstanceOf[G8] && y.asInstanceOf[G8].i == z; case H8(z) => y.isInstanceOf[H8] && y.asInstanceOf[H8].b == z; case I8(z) => y.isInstanceOf[I8] && y.asInstanceOf[I8].i == z; case J8(z) => y.isInstanceOf[J8] && y.asInstanceOf[J8].b == z
    }
  } // EqC90
  object EqC100 extends Eq[C100] {
    import C100._
    def eqv(x: C100, y: C100): Boolean = x match {
      case A0(z) => y.isInstanceOf[A0] && y.asInstanceOf[A0].i == z; case B0(z) => y.isInstanceOf[B0] && y.asInstanceOf[B0].b == z; case S0(z) => y.isInstanceOf[S0] && y.asInstanceOf[S0].i == z; case D0(z) => y.isInstanceOf[D0] && y.asInstanceOf[D0].b == z; case E0(z) => y.isInstanceOf[E0] && y.asInstanceOf[E0].i == z; case F0(z) => y.isInstanceOf[F0] && y.asInstanceOf[F0].b == z; case G0(z) => y.isInstanceOf[G0] && y.asInstanceOf[G0].i == z; case H0(z) => y.isInstanceOf[H0] && y.asInstanceOf[H0].b == z; case I0(z) => y.isInstanceOf[I0] && y.asInstanceOf[I0].i == z; case J0(z) => y.isInstanceOf[J0] && y.asInstanceOf[J0].b == z
      case A1(z) => y.isInstanceOf[A1] && y.asInstanceOf[A1].i == z; case B1(z) => y.isInstanceOf[B1] && y.asInstanceOf[B1].b == z; case S1(z) => y.isInstanceOf[S1] && y.asInstanceOf[S1].i == z; case D1(z) => y.isInstanceOf[D1] && y.asInstanceOf[D1].b == z; case E1(z) => y.isInstanceOf[E1] && y.asInstanceOf[E1].i == z; case F1(z) => y.isInstanceOf[F1] && y.asInstanceOf[F1].b == z; case G1(z) => y.isInstanceOf[G1] && y.asInstanceOf[G1].i == z; case H1(z) => y.isInstanceOf[H1] && y.asInstanceOf[H1].b == z; case I1(z) => y.isInstanceOf[I1] && y.asInstanceOf[I1].i == z; case J1(z) => y.isInstanceOf[J1] && y.asInstanceOf[J1].b == z
      case A2(z) => y.isInstanceOf[A2] && y.asInstanceOf[A2].i == z; case B2(z) => y.isInstanceOf[B2] && y.asInstanceOf[B2].b == z; case S2(z) => y.isInstanceOf[S2] && y.asInstanceOf[S2].i == z; case D2(z) => y.isInstanceOf[D2] && y.asInstanceOf[D2].b == z; case E2(z) => y.isInstanceOf[E2] && y.asInstanceOf[E2].i == z; case F2(z) => y.isInstanceOf[F2] && y.asInstanceOf[F2].b == z; case G2(z) => y.isInstanceOf[G2] && y.asInstanceOf[G2].i == z; case H2(z) => y.isInstanceOf[H2] && y.asInstanceOf[H2].b == z; case I2(z) => y.isInstanceOf[I2] && y.asInstanceOf[I2].i == z; case J2(z) => y.isInstanceOf[J2] && y.asInstanceOf[J2].b == z
      case A3(z) => y.isInstanceOf[A3] && y.asInstanceOf[A3].i == z; case B3(z) => y.isInstanceOf[B3] && y.asInstanceOf[B3].b == z; case S3(z) => y.isInstanceOf[S3] && y.asInstanceOf[S3].i == z; case D3(z) => y.isInstanceOf[D3] && y.asInstanceOf[D3].b == z; case E3(z) => y.isInstanceOf[E3] && y.asInstanceOf[E3].i == z; case F3(z) => y.isInstanceOf[F3] && y.asInstanceOf[F3].b == z; case G3(z) => y.isInstanceOf[G3] && y.asInstanceOf[G3].i == z; case H3(z) => y.isInstanceOf[H3] && y.asInstanceOf[H3].b == z; case I3(z) => y.isInstanceOf[I3] && y.asInstanceOf[I3].i == z; case J3(z) => y.isInstanceOf[J3] && y.asInstanceOf[J3].b == z
      case A4(z) => y.isInstanceOf[A4] && y.asInstanceOf[A4].i == z; case B4(z) => y.isInstanceOf[B4] && y.asInstanceOf[B4].b == z; case S4(z) => y.isInstanceOf[S4] && y.asInstanceOf[S4].i == z; case D4(z) => y.isInstanceOf[D4] && y.asInstanceOf[D4].b == z; case E4(z) => y.isInstanceOf[E4] && y.asInstanceOf[E4].i == z; case F4(z) => y.isInstanceOf[F4] && y.asInstanceOf[F4].b == z; case G4(z) => y.isInstanceOf[G4] && y.asInstanceOf[G4].i == z; case H4(z) => y.isInstanceOf[H4] && y.asInstanceOf[H4].b == z; case I4(z) => y.isInstanceOf[I4] && y.asInstanceOf[I4].i == z; case J4(z) => y.isInstanceOf[J4] && y.asInstanceOf[J4].b == z
      case A5(z) => y.isInstanceOf[A5] && y.asInstanceOf[A5].i == z; case B5(z) => y.isInstanceOf[B5] && y.asInstanceOf[B5].b == z; case S5(z) => y.isInstanceOf[S5] && y.asInstanceOf[S5].i == z; case D5(z) => y.isInstanceOf[D5] && y.asInstanceOf[D5].b == z; case E5(z) => y.isInstanceOf[E5] && y.asInstanceOf[E5].i == z; case F5(z) => y.isInstanceOf[F5] && y.asInstanceOf[F5].b == z; case G5(z) => y.isInstanceOf[G5] && y.asInstanceOf[G5].i == z; case H5(z) => y.isInstanceOf[H5] && y.asInstanceOf[H5].b == z; case I5(z) => y.isInstanceOf[I5] && y.asInstanceOf[I5].i == z; case J5(z) => y.isInstanceOf[J5] && y.asInstanceOf[J5].b == z
      case A6(z) => y.isInstanceOf[A6] && y.asInstanceOf[A6].i == z; case B6(z) => y.isInstanceOf[B6] && y.asInstanceOf[B6].b == z; case S6(z) => y.isInstanceOf[S6] && y.asInstanceOf[S6].i == z; case D6(z) => y.isInstanceOf[D6] && y.asInstanceOf[D6].b == z; case E6(z) => y.isInstanceOf[E6] && y.asInstanceOf[E6].i == z; case F6(z) => y.isInstanceOf[F6] && y.asInstanceOf[F6].b == z; case G6(z) => y.isInstanceOf[G6] && y.asInstanceOf[G6].i == z; case H6(z) => y.isInstanceOf[H6] && y.asInstanceOf[H6].b == z; case I6(z) => y.isInstanceOf[I6] && y.asInstanceOf[I6].i == z; case J6(z) => y.isInstanceOf[J6] && y.asInstanceOf[J6].b == z
      case A7(z) => y.isInstanceOf[A7] && y.asInstanceOf[A7].i == z; case B7(z) => y.isInstanceOf[B7] && y.asInstanceOf[B7].b == z; case S7(z) => y.isInstanceOf[S7] && y.asInstanceOf[S7].i == z; case D7(z) => y.isInstanceOf[D7] && y.asInstanceOf[D7].b == z; case E7(z) => y.isInstanceOf[E7] && y.asInstanceOf[E7].i == z; case F7(z) => y.isInstanceOf[F7] && y.asInstanceOf[F7].b == z; case G7(z) => y.isInstanceOf[G7] && y.asInstanceOf[G7].i == z; case H7(z) => y.isInstanceOf[H7] && y.asInstanceOf[H7].b == z; case I7(z) => y.isInstanceOf[I7] && y.asInstanceOf[I7].i == z; case J7(z) => y.isInstanceOf[J7] && y.asInstanceOf[J7].b == z
      case A8(z) => y.isInstanceOf[A8] && y.asInstanceOf[A8].i == z; case B8(z) => y.isInstanceOf[B8] && y.asInstanceOf[B8].b == z; case S8(z) => y.isInstanceOf[S8] && y.asInstanceOf[S8].i == z; case D8(z) => y.isInstanceOf[D8] && y.asInstanceOf[D8].b == z; case E8(z) => y.isInstanceOf[E8] && y.asInstanceOf[E8].i == z; case F8(z) => y.isInstanceOf[F8] && y.asInstanceOf[F8].b == z; case G8(z) => y.isInstanceOf[G8] && y.asInstanceOf[G8].i == z; case H8(z) => y.isInstanceOf[H8] && y.asInstanceOf[H8].b == z; case I8(z) => y.isInstanceOf[I8] && y.asInstanceOf[I8].i == z; case J8(z) => y.isInstanceOf[J8] && y.asInstanceOf[J8].b == z
      case A9(z) => y.isInstanceOf[A9] && y.asInstanceOf[A9].i == z; case B9(z) => y.isInstanceOf[B9] && y.asInstanceOf[B9].b == z; case S9(z) => y.isInstanceOf[S9] && y.asInstanceOf[S9].i == z; case D9(z) => y.isInstanceOf[D9] && y.asInstanceOf[D9].b == z; case E9(z) => y.isInstanceOf[E9] && y.asInstanceOf[E9].i == z; case F9(z) => y.isInstanceOf[F9] && y.asInstanceOf[F9].b == z; case G9(z) => y.isInstanceOf[G9] && y.asInstanceOf[G9].i == z; case H9(z) => y.isInstanceOf[H9] && y.asInstanceOf[H9].b == z; case I9(z) => y.isInstanceOf[I9] && y.asInstanceOf[I9].i == z; case J9(z) => y.isInstanceOf[J9] && y.asInstanceOf[J9].b == z
    }
  } // EqC100

  object FunctorPK0 extends Functor[PK0] {
    def map[A, B](fa: PK0[A])(f: A => B): PK0[B] = PK0()
  } // FunctorPK0
  object FunctorPK1 extends Functor[PK1] {
    def map[A, B](fa: PK1[A])(f: A => B): PK1[B] = PK1[B](fa.a0)
  } // FunctorPK1
  object FunctorPK2 extends Functor[PK2] {
    def map[A, B](fa: PK2[A])(f: A => B): PK2[B] = PK2[B](fa.a0, f(fa.b0))
  } // FunctorPK2
  object FunctorPK3 extends Functor[PK3] {
    def map[A, B](fa: PK3[A])(f: A => B): PK3[B] = PK3[B](fa.a0, f(fa.b0), fa.c0)
  } // FunctorPK3
  object FunctorPK4 extends Functor[PK4] {
    def map[A, B](fa: PK4[A])(f: A => B): PK4[B] = PK4[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0))
  } // FunctorPK4
  object FunctorPK5 extends Functor[PK5] {
    def map[A, B](fa: PK5[A])(f: A => B): PK5[B] = PK5[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0)
  } // FunctorPK5
  object FunctorPK6 extends Functor[PK6] {
    def map[A, B](fa: PK6[A])(f: A => B): PK6[B] = PK6[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0))
  } // FunctorPK6
  object FunctorPK7 extends Functor[PK7] {
    def map[A, B](fa: PK7[A])(f: A => B): PK7[B] = PK7[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0)
  } // FunctorPK7
  object FunctorPK8 extends Functor[PK8] {
    def map[A, B](fa: PK8[A])(f: A => B): PK8[B] = PK8[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0))
  } // FunctorPK8
  object FunctorPK9 extends Functor[PK9] {
    def map[A, B](fa: PK9[A])(f: A => B): PK9[B] = PK9[B](fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0)
  } // FunctorPK9
  object FunctorPK10 extends Functor[PK10] {
    def map[A, B](fa: PK10[A])(f: A => B): PK10[B] =
      PK10[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0)
      )
  } // FunctorPK10
  object FunctorPK20 extends Functor[PK20] {
    def map[A, B](fa: PK20[A])(f: A => B): PK20[B] =
      PK20[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1)
      )
  } // FunctorPK20
  object FunctorPK30 extends Functor[PK30] {
    def map[A, B](fa: PK30[A])(f: A => B): PK30[B] =
      PK30[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1),
        fa.a2, f(fa.b2), fa.c2, f(fa.d2), fa.e2, f(fa.f2), fa.g2, f(fa.h2), fa.i2, f(fa.j2)
      )
  } // FunctorPK30
  object FunctorPK40 extends Functor[PK40] {
    def map[A, B](fa: PK40[A])(f: A => B): PK40[B] =
      PK40[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1),
        fa.a2, f(fa.b2), fa.c2, f(fa.d2), fa.e2, f(fa.f2), fa.g2, f(fa.h2), fa.i2, f(fa.j2),
        fa.a3, f(fa.b3), fa.c3, f(fa.d3), fa.e3, f(fa.f3), fa.g3, f(fa.h3), fa.i3, f(fa.j3)
      )
  } // FunctorPK40
  object FunctorPK50 extends Functor[PK50] {
    def map[A, B](fa: PK50[A])(f: A => B): PK50[B] =
      PK50[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1),
        fa.a2, f(fa.b2), fa.c2, f(fa.d2), fa.e2, f(fa.f2), fa.g2, f(fa.h2), fa.i2, f(fa.j2),
        fa.a3, f(fa.b3), fa.c3, f(fa.d3), fa.e3, f(fa.f3), fa.g3, f(fa.h3), fa.i3, f(fa.j3),
        fa.a4, f(fa.b4), fa.c4, f(fa.d4), fa.e4, f(fa.f4), fa.g4, f(fa.h4), fa.i4, f(fa.j4)
      )
  } // FunctorPK50
  object FunctorPK60 extends Functor[PK60] {
    def map[A, B](fa: PK60[A])(f: A => B): PK60[B] =
      PK60[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1),
        fa.a2, f(fa.b2), fa.c2, f(fa.d2), fa.e2, f(fa.f2), fa.g2, f(fa.h2), fa.i2, f(fa.j2),
        fa.a3, f(fa.b3), fa.c3, f(fa.d3), fa.e3, f(fa.f3), fa.g3, f(fa.h3), fa.i3, f(fa.j3),
        fa.a4, f(fa.b4), fa.c4, f(fa.d4), fa.e4, f(fa.f4), fa.g4, f(fa.h4), fa.i4, f(fa.j4),
        fa.a5, f(fa.b5), fa.c5, f(fa.d5), fa.e5, f(fa.f5), fa.g5, f(fa.h5), fa.i5, f(fa.j5)
      )
  } // FunctorPK60
  object FunctorPK70 extends Functor[PK70] {
    def map[A, B](fa: PK70[A])(f: A => B): PK70[B] =
      PK70[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1),
        fa.a2, f(fa.b2), fa.c2, f(fa.d2), fa.e2, f(fa.f2), fa.g2, f(fa.h2), fa.i2, f(fa.j2),
        fa.a3, f(fa.b3), fa.c3, f(fa.d3), fa.e3, f(fa.f3), fa.g3, f(fa.h3), fa.i3, f(fa.j3),
        fa.a4, f(fa.b4), fa.c4, f(fa.d4), fa.e4, f(fa.f4), fa.g4, f(fa.h4), fa.i4, f(fa.j4),
        fa.a5, f(fa.b5), fa.c5, f(fa.d5), fa.e5, f(fa.f5), fa.g5, f(fa.h5), fa.i5, f(fa.j5),
        fa.a6, f(fa.b6), fa.c6, f(fa.d6), fa.e6, f(fa.f6), fa.g6, f(fa.h6), fa.i6, f(fa.j6)
      )
  } // FunctorPK70
  object FunctorPK80 extends Functor[PK80] {
    def map[A, B](fa: PK80[A])(f: A => B): PK80[B] =
      PK80[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1),
        fa.a2, f(fa.b2), fa.c2, f(fa.d2), fa.e2, f(fa.f2), fa.g2, f(fa.h2), fa.i2, f(fa.j2),
        fa.a3, f(fa.b3), fa.c3, f(fa.d3), fa.e3, f(fa.f3), fa.g3, f(fa.h3), fa.i3, f(fa.j3),
        fa.a4, f(fa.b4), fa.c4, f(fa.d4), fa.e4, f(fa.f4), fa.g4, f(fa.h4), fa.i4, f(fa.j4),
        fa.a5, f(fa.b5), fa.c5, f(fa.d5), fa.e5, f(fa.f5), fa.g5, f(fa.h5), fa.i5, f(fa.j5),
        fa.a6, f(fa.b6), fa.c6, f(fa.d6), fa.e6, f(fa.f6), fa.g6, f(fa.h6), fa.i6, f(fa.j6),
        fa.a7, f(fa.b7), fa.c7, f(fa.d7), fa.e7, f(fa.f7), fa.g7, f(fa.h7), fa.i7, f(fa.j7)
      )
  } // FunctorPK80
  object FunctorPK90 extends Functor[PK90] {
    def map[A, B](fa: PK90[A])(f: A => B): PK90[B] =
      PK90[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1),
        fa.a2, f(fa.b2), fa.c2, f(fa.d2), fa.e2, f(fa.f2), fa.g2, f(fa.h2), fa.i2, f(fa.j2),
        fa.a3, f(fa.b3), fa.c3, f(fa.d3), fa.e3, f(fa.f3), fa.g3, f(fa.h3), fa.i3, f(fa.j3),
        fa.a4, f(fa.b4), fa.c4, f(fa.d4), fa.e4, f(fa.f4), fa.g4, f(fa.h4), fa.i4, f(fa.j4),
        fa.a5, f(fa.b5), fa.c5, f(fa.d5), fa.e5, f(fa.f5), fa.g5, f(fa.h5), fa.i5, f(fa.j5),
        fa.a6, f(fa.b6), fa.c6, f(fa.d6), fa.e6, f(fa.f6), fa.g6, f(fa.h6), fa.i6, f(fa.j6),
        fa.a7, f(fa.b7), fa.c7, f(fa.d7), fa.e7, f(fa.f7), fa.g7, f(fa.h7), fa.i7, f(fa.j7),
        fa.a8, f(fa.b8), fa.c8, f(fa.d8), fa.e8, f(fa.f8), fa.g8, f(fa.h8), fa.i8, f(fa.j8)
      )
  } // FunctorPK90
  object FunctorPK100 extends Functor[PK100] {
    def map[A, B](fa: PK100[A])(f: A => B): PK100[B] =
      PK100[B](
        fa.a0, f(fa.b0), fa.c0, f(fa.d0), fa.e0, f(fa.f0), fa.g0, f(fa.h0), fa.i0, f(fa.j0),
        fa.a1, f(fa.b1), fa.c1, f(fa.d1), fa.e1, f(fa.f1), fa.g1, f(fa.h1), fa.i1, f(fa.j1),
        fa.a2, f(fa.b2), fa.c2, f(fa.d2), fa.e2, f(fa.f2), fa.g2, f(fa.h2), fa.i2, f(fa.j2),
        fa.a3, f(fa.b3), fa.c3, f(fa.d3), fa.e3, f(fa.f3), fa.g3, f(fa.h3), fa.i3, f(fa.j3),
        fa.a4, f(fa.b4), fa.c4, f(fa.d4), fa.e4, f(fa.f4), fa.g4, f(fa.h4), fa.i4, f(fa.j4),
        fa.a5, f(fa.b5), fa.c5, f(fa.d5), fa.e5, f(fa.f5), fa.g5, f(fa.h5), fa.i5, f(fa.j5),
        fa.a6, f(fa.b6), fa.c6, f(fa.d6), fa.e6, f(fa.f6), fa.g6, f(fa.h6), fa.i6, f(fa.j6),
        fa.a7, f(fa.b7), fa.c7, f(fa.d7), fa.e7, f(fa.f7), fa.g7, f(fa.h7), fa.i7, f(fa.j7),
        fa.a8, f(fa.b8), fa.c8, f(fa.d8), fa.e8, f(fa.f8), fa.g8, f(fa.h8), fa.i8, f(fa.j8),
        fa.a9, f(fa.b9), fa.c9, f(fa.d9), fa.e9, f(fa.f9), fa.g9, f(fa.h9), fa.i9, f(fa.j9)
      )
  } // FunctorPK100

  object FunctorCK1 extends Functor[CK1] {
    import CK1._
    def map[A, B](fa: CK1[A])(f: A => B): CK1[B] = fa match {
      case A0(x) => A0(f(x))
    }
  } // FunctorCK1
  object FunctorCK2 extends Functor[CK2] {
    import CK2._
    def map[A, B](fa: CK2[A])(f: A => B): CK2[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x)
    }
  } // FunctorCK2
  object FunctorCK3 extends Functor[CK3] {
    import CK3._
    def map[A, B](fa: CK3[A])(f: A => B): CK3[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x))
    }
  } // FunctorCK3
  object FunctorCK4 extends Functor[CK4] {
    import CK4._
    def map[A, B](fa: CK4[A])(f: A => B): CK4[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x)
    }
  } // FunctorCK4
  object FunctorCK5 extends Functor[CK5] {
    import CK5._
    def map[A, B](fa: CK5[A])(f: A => B): CK5[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x))
    }
  } // FunctorCK5
  object FunctorCK6 extends Functor[CK6] {
    import CK6._
    def map[A, B](fa: CK6[A])(f: A => B): CK6[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x)
    }
  } // FunctorCK6
  object FunctorCK7 extends Functor[CK7] {
    import CK7._
    def map[A, B](fa: CK7[A])(f: A => B): CK7[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x))
    }
  } // FunctorCK7
  object FunctorCK8 extends Functor[CK8] {
    import CK8._
    def map[A, B](fa: CK8[A])(f: A => B): CK8[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x)
    }
  } // FunctorCK8
  object FunctorCK9 extends Functor[CK9] {
    import CK9._
    def map[A, B](fa: CK9[A])(f: A => B): CK9[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x))
    }
  } // FunctorCK9
  object FunctorCK10 extends Functor[CK10] {
    import CK10._
    def map[A, B](fa: CK10[A])(f: A => B): CK10[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
    }
  } // FunctorCK10
  object FunctorCK20 extends Functor[CK20] {
    import CK20._
    def map[A, B](fa: CK20[A])(f: A => B): CK20[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
    }
  } // FunctorCK20
  object FunctorCK30 extends Functor[CK30] {
    import CK30._
    def map[A, B](fa: CK30[A])(f: A => B): CK30[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
      case A2(x) => A2(f(x)); case B2(x) => B2(x); case S2(x) => S2(f(x)); case D2(x) => D2(x); case E2(x) => E2(f(x)); case F2(x) => F2(x); case G2(x) => G2(f(x)); case H2(x) => H2(x); case I2(x) => I2(f(x)); case J2(x) => J2(x)
    }
  } // FunctorCK30
  object FunctorCK40 extends Functor[CK40] {
    import CK40._
    def map[A, B](fa: CK40[A])(f: A => B): CK40[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
      case A2(x) => A2(f(x)); case B2(x) => B2(x); case S2(x) => S2(f(x)); case D2(x) => D2(x); case E2(x) => E2(f(x)); case F2(x) => F2(x); case G2(x) => G2(f(x)); case H2(x) => H2(x); case I2(x) => I2(f(x)); case J2(x) => J2(x)
      case A3(x) => A3(f(x)); case B3(x) => B3(x); case S3(x) => S3(f(x)); case D3(x) => D3(x); case E3(x) => E3(f(x)); case F3(x) => F3(x); case G3(x) => G3(f(x)); case H3(x) => H3(x); case I3(x) => I3(f(x)); case J3(x) => J3(x)
    }
  } // FunctorCK40
  object FunctorCK50 extends Functor[CK50] {
    import CK50._
    def map[A, B](fa: CK50[A])(f: A => B): CK50[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
      case A2(x) => A2(f(x)); case B2(x) => B2(x); case S2(x) => S2(f(x)); case D2(x) => D2(x); case E2(x) => E2(f(x)); case F2(x) => F2(x); case G2(x) => G2(f(x)); case H2(x) => H2(x); case I2(x) => I2(f(x)); case J2(x) => J2(x)
      case A3(x) => A3(f(x)); case B3(x) => B3(x); case S3(x) => S3(f(x)); case D3(x) => D3(x); case E3(x) => E3(f(x)); case F3(x) => F3(x); case G3(x) => G3(f(x)); case H3(x) => H3(x); case I3(x) => I3(f(x)); case J3(x) => J3(x)
      case A4(x) => A4(f(x)); case B4(x) => B4(x); case S4(x) => S4(f(x)); case D4(x) => D4(x); case E4(x) => E4(f(x)); case F4(x) => F4(x); case G4(x) => G4(f(x)); case H4(x) => H4(x); case I4(x) => I4(f(x)); case J4(x) => J4(x)
    }
  } // FunctorCK50
  object FunctorCK60 extends Functor[CK60] {
    import CK60._
    def map[A, B](fa: CK60[A])(f: A => B): CK60[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
      case A2(x) => A2(f(x)); case B2(x) => B2(x); case S2(x) => S2(f(x)); case D2(x) => D2(x); case E2(x) => E2(f(x)); case F2(x) => F2(x); case G2(x) => G2(f(x)); case H2(x) => H2(x); case I2(x) => I2(f(x)); case J2(x) => J2(x)
      case A3(x) => A3(f(x)); case B3(x) => B3(x); case S3(x) => S3(f(x)); case D3(x) => D3(x); case E3(x) => E3(f(x)); case F3(x) => F3(x); case G3(x) => G3(f(x)); case H3(x) => H3(x); case I3(x) => I3(f(x)); case J3(x) => J3(x)
      case A4(x) => A4(f(x)); case B4(x) => B4(x); case S4(x) => S4(f(x)); case D4(x) => D4(x); case E4(x) => E4(f(x)); case F4(x) => F4(x); case G4(x) => G4(f(x)); case H4(x) => H4(x); case I4(x) => I4(f(x)); case J4(x) => J4(x)
      case A5(x) => A5(f(x)); case B5(x) => B5(x); case S5(x) => S5(f(x)); case D5(x) => D5(x); case E5(x) => E5(f(x)); case F5(x) => F5(x); case G5(x) => G5(f(x)); case H5(x) => H5(x); case I5(x) => I5(f(x)); case J5(x) => J5(x)
    }
  } // FunctorCK60
  object FunctorCK70 extends Functor[CK70] {
    import CK70._
    def map[A, B](fa: CK70[A])(f: A => B): CK70[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
      case A2(x) => A2(f(x)); case B2(x) => B2(x); case S2(x) => S2(f(x)); case D2(x) => D2(x); case E2(x) => E2(f(x)); case F2(x) => F2(x); case G2(x) => G2(f(x)); case H2(x) => H2(x); case I2(x) => I2(f(x)); case J2(x) => J2(x)
      case A3(x) => A3(f(x)); case B3(x) => B3(x); case S3(x) => S3(f(x)); case D3(x) => D3(x); case E3(x) => E3(f(x)); case F3(x) => F3(x); case G3(x) => G3(f(x)); case H3(x) => H3(x); case I3(x) => I3(f(x)); case J3(x) => J3(x)
      case A4(x) => A4(f(x)); case B4(x) => B4(x); case S4(x) => S4(f(x)); case D4(x) => D4(x); case E4(x) => E4(f(x)); case F4(x) => F4(x); case G4(x) => G4(f(x)); case H4(x) => H4(x); case I4(x) => I4(f(x)); case J4(x) => J4(x)
      case A5(x) => A5(f(x)); case B5(x) => B5(x); case S5(x) => S5(f(x)); case D5(x) => D5(x); case E5(x) => E5(f(x)); case F5(x) => F5(x); case G5(x) => G5(f(x)); case H5(x) => H5(x); case I5(x) => I5(f(x)); case J5(x) => J5(x)
      case A6(x) => A6(f(x)); case B6(x) => B6(x); case S6(x) => S6(f(x)); case D6(x) => D6(x); case E6(x) => E6(f(x)); case F6(x) => F6(x); case G6(x) => G6(f(x)); case H6(x) => H6(x); case I6(x) => I6(f(x)); case J6(x) => J6(x)
    }
  } // FunctorCK70
  object FunctorCK80 extends Functor[CK80] {
    import CK80._
    def map[A, B](fa: CK80[A])(f: A => B): CK80[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
      case A2(x) => A2(f(x)); case B2(x) => B2(x); case S2(x) => S2(f(x)); case D2(x) => D2(x); case E2(x) => E2(f(x)); case F2(x) => F2(x); case G2(x) => G2(f(x)); case H2(x) => H2(x); case I2(x) => I2(f(x)); case J2(x) => J2(x)
      case A3(x) => A3(f(x)); case B3(x) => B3(x); case S3(x) => S3(f(x)); case D3(x) => D3(x); case E3(x) => E3(f(x)); case F3(x) => F3(x); case G3(x) => G3(f(x)); case H3(x) => H3(x); case I3(x) => I3(f(x)); case J3(x) => J3(x)
      case A4(x) => A4(f(x)); case B4(x) => B4(x); case S4(x) => S4(f(x)); case D4(x) => D4(x); case E4(x) => E4(f(x)); case F4(x) => F4(x); case G4(x) => G4(f(x)); case H4(x) => H4(x); case I4(x) => I4(f(x)); case J4(x) => J4(x)
      case A5(x) => A5(f(x)); case B5(x) => B5(x); case S5(x) => S5(f(x)); case D5(x) => D5(x); case E5(x) => E5(f(x)); case F5(x) => F5(x); case G5(x) => G5(f(x)); case H5(x) => H5(x); case I5(x) => I5(f(x)); case J5(x) => J5(x)
      case A6(x) => A6(f(x)); case B6(x) => B6(x); case S6(x) => S6(f(x)); case D6(x) => D6(x); case E6(x) => E6(f(x)); case F6(x) => F6(x); case G6(x) => G6(f(x)); case H6(x) => H6(x); case I6(x) => I6(f(x)); case J6(x) => J6(x)
      case A7(x) => A7(f(x)); case B7(x) => B7(x); case S7(x) => S7(f(x)); case D7(x) => D7(x); case E7(x) => E7(f(x)); case F7(x) => F7(x); case G7(x) => G7(f(x)); case H7(x) => H7(x); case I7(x) => I7(f(x)); case J7(x) => J7(x)
    }
  } // FunctorCK80
  object FunctorCK90 extends Functor[CK90] {
    import CK90._
    def map[A, B](fa: CK90[A])(f: A => B): CK90[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
      case A2(x) => A2(f(x)); case B2(x) => B2(x); case S2(x) => S2(f(x)); case D2(x) => D2(x); case E2(x) => E2(f(x)); case F2(x) => F2(x); case G2(x) => G2(f(x)); case H2(x) => H2(x); case I2(x) => I2(f(x)); case J2(x) => J2(x)
      case A3(x) => A3(f(x)); case B3(x) => B3(x); case S3(x) => S3(f(x)); case D3(x) => D3(x); case E3(x) => E3(f(x)); case F3(x) => F3(x); case G3(x) => G3(f(x)); case H3(x) => H3(x); case I3(x) => I3(f(x)); case J3(x) => J3(x)
      case A4(x) => A4(f(x)); case B4(x) => B4(x); case S4(x) => S4(f(x)); case D4(x) => D4(x); case E4(x) => E4(f(x)); case F4(x) => F4(x); case G4(x) => G4(f(x)); case H4(x) => H4(x); case I4(x) => I4(f(x)); case J4(x) => J4(x)
      case A5(x) => A5(f(x)); case B5(x) => B5(x); case S5(x) => S5(f(x)); case D5(x) => D5(x); case E5(x) => E5(f(x)); case F5(x) => F5(x); case G5(x) => G5(f(x)); case H5(x) => H5(x); case I5(x) => I5(f(x)); case J5(x) => J5(x)
      case A6(x) => A6(f(x)); case B6(x) => B6(x); case S6(x) => S6(f(x)); case D6(x) => D6(x); case E6(x) => E6(f(x)); case F6(x) => F6(x); case G6(x) => G6(f(x)); case H6(x) => H6(x); case I6(x) => I6(f(x)); case J6(x) => J6(x)
      case A7(x) => A7(f(x)); case B7(x) => B7(x); case S7(x) => S7(f(x)); case D7(x) => D7(x); case E7(x) => E7(f(x)); case F7(x) => F7(x); case G7(x) => G7(f(x)); case H7(x) => H7(x); case I7(x) => I7(f(x)); case J7(x) => J7(x)
      case A8(x) => A8(f(x)); case B8(x) => B8(x); case S8(x) => S8(f(x)); case D8(x) => D8(x); case E8(x) => E8(f(x)); case F8(x) => F8(x); case G8(x) => G8(f(x)); case H8(x) => H8(x); case I8(x) => I8(f(x)); case J8(x) => J8(x)
    }
  } // FunctorCK90
  object FunctorCK100 extends Functor[CK100] {
    import CK100._
    def map[A, B](fa: CK100[A])(f: A => B): CK100[B] = fa match {
      case A0(x) => A0(f(x)); case B0(x) => B0(x); case S0(x) => S0(f(x)); case D0(x) => D0(x); case E0(x) => E0(f(x)); case F0(x) => F0(x); case G0(x) => G0(f(x)); case H0(x) => H0(x); case I0(x) => I0(f(x)); case J0(x) => J0(x)
      case A1(x) => A1(f(x)); case B1(x) => B1(x); case S1(x) => S1(f(x)); case D1(x) => D1(x); case E1(x) => E1(f(x)); case F1(x) => F1(x); case G1(x) => G1(f(x)); case H1(x) => H1(x); case I1(x) => I1(f(x)); case J1(x) => J1(x)
      case A2(x) => A2(f(x)); case B2(x) => B2(x); case S2(x) => S2(f(x)); case D2(x) => D2(x); case E2(x) => E2(f(x)); case F2(x) => F2(x); case G2(x) => G2(f(x)); case H2(x) => H2(x); case I2(x) => I2(f(x)); case J2(x) => J2(x)
      case A3(x) => A3(f(x)); case B3(x) => B3(x); case S3(x) => S3(f(x)); case D3(x) => D3(x); case E3(x) => E3(f(x)); case F3(x) => F3(x); case G3(x) => G3(f(x)); case H3(x) => H3(x); case I3(x) => I3(f(x)); case J3(x) => J3(x)
      case A4(x) => A4(f(x)); case B4(x) => B4(x); case S4(x) => S4(f(x)); case D4(x) => D4(x); case E4(x) => E4(f(x)); case F4(x) => F4(x); case G4(x) => G4(f(x)); case H4(x) => H4(x); case I4(x) => I4(f(x)); case J4(x) => J4(x)
      case A5(x) => A5(f(x)); case B5(x) => B5(x); case S5(x) => S5(f(x)); case D5(x) => D5(x); case E5(x) => E5(f(x)); case F5(x) => F5(x); case G5(x) => G5(f(x)); case H5(x) => H5(x); case I5(x) => I5(f(x)); case J5(x) => J5(x)
      case A6(x) => A6(f(x)); case B6(x) => B6(x); case S6(x) => S6(f(x)); case D6(x) => D6(x); case E6(x) => E6(f(x)); case F6(x) => F6(x); case G6(x) => G6(f(x)); case H6(x) => H6(x); case I6(x) => I6(f(x)); case J6(x) => J6(x)
      case A7(x) => A7(f(x)); case B7(x) => B7(x); case S7(x) => S7(f(x)); case D7(x) => D7(x); case E7(x) => E7(f(x)); case F7(x) => F7(x); case G7(x) => G7(f(x)); case H7(x) => H7(x); case I7(x) => I7(f(x)); case J7(x) => J7(x)
      case A8(x) => A8(f(x)); case B8(x) => B8(x); case S8(x) => S8(f(x)); case D8(x) => D8(x); case E8(x) => E8(f(x)); case F8(x) => F8(x); case G8(x) => G8(f(x)); case H8(x) => H8(x); case I8(x) => I8(f(x)); case J8(x) => J8(x)
      case A9(x) => A9(f(x)); case B9(x) => B9(x); case S9(x) => S9(f(x)); case D9(x) => D9(x); case E9(x) => E9(f(x)); case F9(x) => F9(x); case G9(x) => G9(f(x)); case H9(x) => H9(x); case I9(x) => I9(f(x)); case J9(x) => J9(x)
    }
  } // FunctorCK100

  // for h in EqP0 EqP1 EqP2 EqP3 EqP4 EqP5 EqP6 EqP7 EqP8 EqP9 EqP10 EqP20 EqP30 EqP40 EqP50 EqP60 EqP70 EqP80 EqP90 EqP100 EqC1 EqC2 EqC3 EqC4 EqC5 EqC6 EqC7 EqC8 EqC9 EqC10 EqC20 EqC30 EqC40 EqC50 EqC60 EqC70 EqC80 EqC90 EqC100 FunctorPK0 FunctorPK1 FunctorPK2 FunctorPK3 FunctorPK4 FunctorPK5 FunctorPK6 FunctorPK7 FunctorPK8 FunctorPK9 FunctorPK10 FunctorPK20 FunctorPK30 FunctorPK40 FunctorPK50 FunctorPK60 FunctorPK70 FunctorPK80 FunctorPK90 FunctorPK100 FunctorCK1 FunctorCK2 FunctorCK3 FunctorCK4 FunctorCK5 FunctorCK6 FunctorCK7 FunctorCK8 FunctorCK9 FunctorCK10 FunctorCK20 FunctorCK30 FunctorCK40 FunctorCK50 FunctorCK60 FunctorCK70 FunctorCK80 FunctorCK90 FunctorCK100; do
  //   echo 'import Hand.{Eq, Functor}' > "/home/olivier/workspace/dotty/bench/jmh-dotty/src/main/hand/$h.scala"
  //   cat /home/olivier/workspace/dotty/bench/jmh-dotty/src/main/scala/1.scala | sed -n "/$h /,/$h/p" >> "/home/olivier/workspace/dotty/bench/jmh-dotty/src/main/hand/$h.scala"
  // done
}
*/
