import scala.annotation.tailrec
import scala.collection.mutable.WrappedArray
import scala.compiletime._
import scala.deriving._

// Standard library

object Utils {
  type Id[t] = t
  type Const[c] = [t] =>> c
  case class Wrap[T](t: T)


  type ~>[A[_], B[_]] = [t] => A[t] => B[t]

  inline def summon[T] = implicit match {
    case t: T => t
  }

  inline def summonValues[T] <: Tuple = inline erasedValue[T] match {
    case _: Unit => ()
    case _: (a *: b) => constValue[a] *: summonValues[b]
  }

  inline def summonValuesAsArray[T]: Array[Any] = inline erasedValue[Id[T]] match {
    case _: Unit => Array()
    case _: Tuple1[a] => Array(constValue[a])
    case _: (a, b) => Array(constValue[a], constValue[b])
    case _: (a, b, c) => Array(constValue[a], constValue[b], constValue[c])
    case _: (a, b, c, d) => Array(constValue[a], constValue[b], constValue[c], constValue[d])
    case _: (a, b, c, d, e) => Array(constValue[a], constValue[b], constValue[c], constValue[d], constValue[e])
    // Add fallback for larger sizes
  }

  case class Fix[S[_, _], A](unfix: S[A, Fix[S, A]])
}

case class Labelling[T](label: String, elemLabels: Seq[String])
object Labelling {
  inline implicit def apply[T0](implicit mirror: Mirror { type MirroredType = T0 }): Labelling[T0] =
    Labelling[T0](constValue[mirror.MirroredLabel & String], WrappedArray.make[String](Utils.summonValuesAsArray[mirror.MirroredElemLabels]))
}

// Third party library

sealed trait CompleteOr[T]
case class Complete[T](t: T) extends CompleteOr[T]
case class Continue[T](t: T) extends CompleteOr[T]

object Complete {
  inline def apply[T](c: Boolean)(t: T)(f: T): CompleteOr[T] =
    if(c) Complete(t)
    else Continue(f)
}

import Utils._

abstract class ErasedInstances[FT] {
  def erasedMap(x: Any)(f: (Any, Any) => Any): Any
}

final class ErasedProductInstances[FT](val mirror: Mirror.Product, is0: => Array[Any]) extends ErasedInstances[FT] {
  lazy val is = is0

  inline def toProduct(x: Any): Product = x.asInstanceOf[Product]

  class ArrayProduct(val elems: Array[Any]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
  }

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

final class ErasedCoproductInstances[FT](mirror: Mirror.Sum, is0: => Array[Any]) extends ErasedInstances[FT] {
  lazy val is = is0

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

object K0 {
  type Generic[O] = Mirror { type MirroredType = O ; type MirroredElemTypes }
  type ProductGeneric[O] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes }
  type CoproductGeneric[O] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes }

  def Generic[O](implicit gen: Generic[O]): Generic[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen
  def ProductGeneric[O](implicit gen: ProductGeneric[O]): ProductGeneric[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen
  def CoproductGeneric[O](implicit gen: CoproductGeneric[O]): CoproductGeneric[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen

  type Instances[F[_], T] = ErasedInstances[F[T]]
  type ProductInstances[F[_], T] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_], T] = ErasedCoproductInstances[F[T]]

  def Instances[F[_], T](implicit inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_], T](implicit inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_], T](implicit inst: CoproductInstances[F, T]): inst.type = inst

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

  inline def summonAsArray[F[_], T]: Array[Any] = inline erasedValue[T] match {
    case _: Unit => Array()
    case _: Tuple1[a] => Array(summon[F[a]])
    case _: (a, b) => Array(summon[F[a]], summon[F[b]])
    case _: (a, b, c) => Array(summon[F[a]], summon[F[b]], summon[F[c]])
    case _: (a, b, c, d) => Array(summon[F[a]], summon[F[b]], summon[F[c]], summon[F[d]])
    case _: (a, b, c, d, e) => Array(summon[F[a]], summon[F[b]], summon[F[c]], summon[F[d]], summon[F[e]])
    // Add fallback for larger sizes
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

  implicit object Ops {
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

  inline implicit def mkInstances[F[_], T](implicit gen: Generic[T]): ErasedInstances[F[T]] =
    inline gen match {
      case p: ProductGeneric[T]   => mkProductInstances[F, T](p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](c)
    }

  inline implicit def mkProductInstances[F[_], T](implicit gen: ProductGeneric[T]): ErasedProductInstances[F[T]] =
    new ErasedProductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf

  inline implicit def mkCoproductInstances[F[_], T](implicit gen: CoproductGeneric[T]): ErasedCoproductInstances[F[T]] =
    new ErasedCoproductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf

  inline def derive[F[_], T](gen: Generic[T], pg: ProductInstances[F, T] => F[T], cg: CoproductInstances[F, T] => F[T]): F[T] =
    inline gen match {
      case p: ProductGeneric[T]   => pg(mkProductInstances[F, T](p))
      case c: CoproductGeneric[T] => cg(mkCoproductInstances[F, T](c))
    }
}

object K1 {
  type Generic[O[_]] = Mirror { type MirroredType = O ; type MirroredElemTypes[_] }
  type ProductGeneric[O[_]] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes[_] }
  type CoproductGeneric[O[_]] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes[_] }

  def Generic[O[_]](implicit gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_]](implicit gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_]](implicit gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_]], T[_]] = ErasedInstances[F[T]]
  type ProductInstances[F[_[_]], T[_]] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_[_]], T[_]] = ErasedCoproductInstances[F[T]]

  def Instances[F[_[_]], T[_]](implicit inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_]], T[_]](implicit inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_]], T[_]](implicit inst: CoproductInstances[F, T]): inst.type = inst

  class Dummy
  type Apply[T[_]] = T[Dummy]
  type Unapply[F[_[_]], T] = T match {
    case Wrap[Apply[a]] => F[a]
    case Wrap[Dummy] => F[Id]
    case Wrap[c] => F[Const[c]]
  }

  inline def summon[F[_[_]], T] = implicit match {
    case ft: Unapply[F, Wrap[T]] => ft
  }

  inline def summonAsArray[F[_[_]], T[_]]: Array[Any] = inline erasedValue[Apply[T]] match {
    case _: Unit => Array()
    case _: Tuple1[a] => Array(summon[F, a])
    case _: (a, b) => Array(summon[F, a], summon[F, b])
    case _: (a, b, c) => Array(summon[F, a], summon[F, b], summon[F, c])
    case _: (a, b, c, d) => Array(summon[F, a], summon[F, b], summon[F, c], summon[F, d])
    case _: (a, b, c, d, e) => Array(summon[F, a], summon[F, b], summon[F, c], summon[F, d], summon[F, e])
    // Add fallback for larger sizes
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

  implicit object Ops {
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

  inline implicit def mkInstances[F[_[_]], T[_]](implicit gen: Generic[T]): ErasedInstances[F[T]] =
    inline gen match {
      case p: ProductGeneric[T] => mkProductInstances[F, T](p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](c)
    }

  inline implicit def mkProductInstances[F[_[_]], T[_]](implicit gen: ProductGeneric[T]): ErasedProductInstances[F[T]] =
    new ErasedProductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf

  inline implicit def mkCoproductInstances[F[_[_]], T[_]](implicit gen: CoproductGeneric[T]): ErasedCoproductInstances[F[T]] =
    new ErasedCoproductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf

  inline def derive[F[_[_]], T[_]](gen: Generic[T], pg: ProductInstances[F, T] => F[T], cg: CoproductInstances[F, T] => F[T]): F[T] =
    inline gen match {
      case p: ProductGeneric[T]   => pg(mkProductInstances[F, T](p))
      case c: CoproductGeneric[T] => cg(mkCoproductInstances[F, T](c))
    }

  implicit def mkK1_0[O](implicit k0: K0.ProductGeneric[O]): ProductGeneric[Const[O]] { type MirroredElemTypes = Const[k0.MirroredElemTypes] } = k0.asInstanceOf
}

object K11 {
  type Generic[O[_[_]]] = Mirror { type MirroredType = O ; type MirroredElemTypes[_[_]] }
  type ProductGeneric[O[_[_]]] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes[_[_]] }
  type CoproductGeneric[O[_[_]]] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes[_[_]] }

  def Generic[O[_[_]]](implicit gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_[_]]](implicit gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_[_]]](implicit gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_[_]]], T[_[_]]] = ErasedInstances[F[T]]
  type ProductInstances[F[_[_[_]]], T[_[_]]] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_[_[_]]], T[_[_]]] = ErasedCoproductInstances[F[T]]

  def Instances[F[_[_[_]]], T[_[_]]](implicit inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_[_]]], T[_[_]]](implicit inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_[_]]], T[_[_]]](implicit inst: CoproductInstances[F, T]): inst.type = inst

  type Id[t] = [f[_]] =>> f[t]
  type Const[c] = [f[_]] =>> c

  class Dummy[T]
  type Apply[T[_[_]]] = T[Dummy]
  type Unapply[F[_[_[_]]], T] = T match {
    case Wrap[Apply[a]] => F[a]
    case Wrap[Dummy[a]] => F[Id[a]]
    case Wrap[c] => F[Const[c]]
  }

  inline def summon[F[_[_[_]]], T] = implicit match {
    case ft: Unapply[F, Wrap[T]] => ft
  }

  inline def summonAsArray[F[_[_[_]]], T[_[_]]]: Array[Any] = inline erasedValue[Apply[T]] match {
    case _: Unit => Array()
    case _: Tuple1[a] => Array(summon[F, a])
    case _: (a, b) => Array(summon[F, a], summon[F, b])
    case _: (a, b, c) => Array(summon[F, a], summon[F, b], summon[F, c])
    case _: (a, b, c, d) => Array(summon[F, a], summon[F, b], summon[F, c], summon[F, d])
    case _: (a, b, c, d, e) => Array(summon[F, a], summon[F, b], summon[F, c], summon[F, d], summon[F, e])
    // Add fallback for larger sizes
  }

  implicit object Ops {
    inline def (inst: Instances[F, T]) map[F[_[_[_]]], T[_[_]], A[_], R[_]](x: T[A])(f: [t[_[_]]] => (F[t], t[A]) => t[R]): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def (inst: ProductInstances[F, T]) construct [F[_[_[_]]], T[_[_]], R[_]] (f: [t[_[_]]] => F[t] => t[R]): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) map2 [F[_[_[_]]], T[_[_]], A[_], B[_], R[_]] (x: T[A], y: T[B])(f: [t[_[_]]] => (F[t], t[A], t[B]) => t[R]): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft [F[_[_[_]]], T[_[_]], A[_], Acc] (x: T[A])(i: Acc)(f: [t[_[_]]] => (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_[_[_]]], T[_[_]], A[_], B[_], Acc] (x: T[A], y: T[B])(i: Acc)(f: [t[_[_]]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def (inst: CoproductInstances[F, T]) fold [F[_[_[_]]], T[_[_]], A[_], R] (x: T[A])(f: [t[_[_]]] => (F[t], t[A]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold2 [F[_[_[_]]], T[_[_]], A[_], B[_], R] (x: T[A], y: T[B])(a: => R)(f: [t[_[_]]] => (F[t], t[A], t[B]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline implicit def mkInstances[F[_[_[_]]], T[_[_]]](implicit gen: Generic[T]): ErasedInstances[F[T]] =
    inline gen match {
      case p: Mirror.Product   => mkProductInstances[F, T](gen.asInstanceOf[ProductGeneric[T] { type MirroredElemTypes = gen.MirroredElemTypes }])
      case c: Mirror.Sum => mkCoproductInstances[F, T](gen.asInstanceOf[CoproductGeneric[T] { type MirroredElemTypes = gen.MirroredElemTypes }])
    }

  inline implicit def mkProductInstances[F[_[_[_]]], T[_[_]]](implicit gen: ProductGeneric[T]): ErasedProductInstances[F[T]] =
    new ErasedProductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf

  inline implicit def mkCoproductInstances[F[_[_[_]]], T[_[_]]](implicit gen: CoproductGeneric[T]): ErasedCoproductInstances[F[T]] =
    new ErasedCoproductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf
}

object K2 {
  type Generic[O[_, _]] = Mirror { type MirroredType = O ; type MirroredElemTypes[_, _] }
  type ProductGeneric[O[_, _]] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes[_, _] }
  type CoproductGeneric[O[_, _]] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes[_, _] }

  def Generic[O[_, _]](implicit gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_, _]](implicit gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_, _]](implicit gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_, _]], T[_, _]] = ErasedInstances[F[T]]
  type ProductInstances[F[_[_, _]], T[_, _]] = ErasedProductInstances[F[T]]
  type CoproductInstances[F[_[_, _]], T[_, _]] = ErasedCoproductInstances[F[T]]

  def Instances[F[_[_, _]], T[_, _]](implicit inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_, _]], T[_, _]](implicit inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_[_, _]], T[_, _]](implicit inst: CoproductInstances[F, T]): inst.type = inst

  type Id1[t, u] = t
  type Id2[t, u] = u
  type Const[c] = [t, u] =>> c

  class Dummy1
  class Dummy2
  type Apply[T[_, _]] = T[Dummy1, Dummy2]
  type Unapply[F[_[_, _]], T] = T match {
    case Wrap[Apply[a]] => F[a]
    case Wrap[Dummy1] => F[Id1]
    case Wrap[Dummy2] => F[Id2]
    case Wrap[c] => F[Const[c]]
  }

  inline def summon[F[_[_, _]], T] = implicit match {
    case ft: Unapply[F, Wrap[T]] => ft
  }

  inline def summonAsArray[F[_[_, _]], T[_, _]]: Array[Any] = inline erasedValue[Apply[T]] match {
    case _: Unit => Array()
    case _: Tuple1[a] => Array(summon[F, a])
    case _: (a, b) => Array(summon[F, a], summon[F, b])
    case _: (a, b, c) => Array(summon[F, a], summon[F, b], summon[F, c])
    case _: (a, b, c, d) => Array(summon[F, a], summon[F, b], summon[F, c], summon[F, d])
    case _: (a, b, c, d, e) => Array(summon[F, a], summon[F, b], summon[F, c], summon[F, d], summon[F, e])
    // Add fallback for larger sizes
  }

  implicit object Ops {
    inline def (inst: Instances[F, T]) map[F[_[_, _]], T[_, _], A, B, R, S](x: T[A, B])(f: [t[_, _]] => (F[t], t[A, B]) => t[R, S]): T[R, S] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def (inst: ProductInstances[F, T]) construct [F[_[_, _]], T[_, _], R, S] (f: [t[_, _]] => F[t] => t[R, S]): T[R, S] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) map2 [F[_[_, _]], T[_, _], A, B, C, D, R, S] (x: T[A, B], y: T[C, D])(f: [t[_, _]] => (F[t], t[A, B], t[C, D]) => t[R, S]): T[R, S] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft [F[_[_, _]], T[_, _], A, B, Acc] (x: T[A, B])(i: Acc)(f: [t[_, _]] => (Acc, F[t], t[A, B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def (inst: ProductInstances[F, T]) foldLeft2 [F[_[_, _]], T[_, _], A, B, C, D, Acc] (x: T[A, B], y: T[C, D])(i: Acc)(f: [t[_, _]] => (Acc, F[t], t[A, B], t[C, D]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf

    inline def (inst: CoproductInstances[F, T]) fold [F[_[_, _]], T[_, _], A, B, R] (x: T[A, B])(f: [t[_, _]] => (F[t], t[A, B]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def (inst: CoproductInstances[F, T]) fold2 [F[_[_, _]], T[_, _], A, B, C, D, R] (x: T[A, B], y: T[C, D])(a: => R)(f: [t[_, _]] => (F[t], t[A, B], t[C, D]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
  }

  inline implicit def mkInstances[F[_[_, _]], T[_, _]](implicit gen: Generic[T]): ErasedInstances[F[T]] =
    inline gen match {
      case p: Mirror.Product   => mkProductInstances[F, T](gen.asInstanceOf[ProductGeneric[T] { type MirroredElemTypes = gen.MirroredElemTypes }])
      case c: Mirror.Sum => mkCoproductInstances[F, T](gen.asInstanceOf[CoproductGeneric[T] { type MirroredElemTypes = gen.MirroredElemTypes }])
    }

  inline implicit def mkProductInstances[F[_[_, _]], T[_, _]](implicit gen: ProductGeneric[T]): ErasedProductInstances[F[T]] =
    new ErasedProductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf

  inline implicit def mkCoproductInstances[F[_[_, _]], T[_, _]](implicit gen: CoproductGeneric[T]): ErasedCoproductInstances[F[T]] =
    new ErasedCoproductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf
}

// Type class definitions

trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

object Monoid {
  inline def apply[A](implicit ma: Monoid[A]): Monoid[A] = ma

  implicit val monoidUnit: Monoid[Unit] = new Monoid[Unit] {
    def empty: Unit = ()
    def combine(x: Unit, y: Unit): Unit = ()
  }
  implicit val monoidBoolean: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = false
    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }
  implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x+y
  }
  implicit val monoidString: Monoid[String] = new Monoid[String] {
    def empty: String = ""
    def combine(x: String, y: String): String = x+y
  }

  implicit def monoidGen[A](implicit inst: K0.ProductInstances[Monoid, A]): Monoid[A] =
    new Monoid[A] {
      def empty: A = inst.construct([t] => (ma: Monoid[t]) => ma.empty)
      def combine(x: A, y: A): A = inst.map2(x, y)([t] => (mt: Monoid[t], t0: t, t1: t) => mt.combine(t0, t1))
    }

  inline def derived[A](implicit gen: K0.ProductGeneric[A]): Monoid[A] =
    monoidGen(K0.mkProductInstances[Monoid, A](gen))
}

trait Eq[A] {
  def eqv(x: A, y: A): Boolean
}

object Eq {
  inline def apply[A](implicit ea: Eq[A]): Eq[A] = ea

  implicit val eqUnit: Eq[Unit] = new Eq[Unit] {
    def eqv(x: Unit, y: Unit): Boolean = true
  }
  implicit val eqBoolean: Eq[Boolean] = new Eq[Boolean] {
    def eqv(x: Boolean, y: Boolean): Boolean = x == y
  }
  implicit val eqInt: Eq[Int] = new Eq[Int] {
    def eqv(x: Int, y: Int): Boolean = x == y
  }
  implicit val eqString: Eq[String] = new Eq[String] {
    def eqv(x: String, y: String): Boolean = x == y
  }

  implicit def eqGen[A](implicit inst: => K0.ProductInstances[Eq, A]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean)(
        [t] => (acc: Boolean, eqt: Eq[t], t0: t, t1: t) => Complete(!eqt.eqv(t0, t1))(false)(true)
      )
    }

  implicit def eqGenC[A](implicit inst: => K0.CoproductInstances[Eq, A]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false)(
        [t] => (eqt: Eq[t], t0: t, t1: t) => eqt.eqv(t0, t1)
      )
    }

  inline def derived[A](implicit gen: K0.Generic[A]): Eq[A] = K0.derive(gen, eqGen, eqGenC)
}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  inline def apply[F[_]](implicit ff: Functor[F]): Functor[F] = ff

  implicit val functorId: Functor[Id] = new Functor[Id] {
    def map[A, B](a: A)(f: A => B): B = f(a)
  }

  implicit def functorNested[F[_], G[_]](implicit ff: Functor[F], fg: Functor[G]): Functor[[t] =>> F[G[t]]] =
    new Functor[[t] =>> F[G[t]]] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = ff.map(fga)(ga => fg.map(ga)(f))
    }

  implicit def functorGen[F[_]](implicit inst: => K1.Instances[Functor, F]): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] = inst.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) => ft.map(ta)(f))
    }

  implicit def functorConst[T]: Functor[Const[T]] = new Functor[Const[T]] {
    def map[A, B](t: T)(f: A => B): T = t
  }

  inline def derived[F[_]](implicit gen: K1.Generic[F]): Functor[F] =
    functorGen(K1.mkInstances[Functor, F](gen))
}

trait FunctorK[H[_[_]]] {
  def mapK[A[_], B[_]](af: H[A])(f: A ~> B): H[B]
}

object FunctorK {
  inline def apply[H[_[_]]](implicit fh: FunctorK[H]): FunctorK[H] = fh

  implicit def functorKApplyTo[T]: FunctorK[K11.Id[T]] =
    new FunctorK[K11.Id[T]] {
      def mapK[A[_], B[_]](at: A[T])(f: A ~> B): B[T] = f(at)
    }


  implicit def functorKGen[H[_[_]]](implicit inst: => K11.Instances[FunctorK, H]): FunctorK[H] =
    new FunctorK[H] {
      def mapK[A[_], B[_]](ha: H[A])(f: A ~> B): H[B] =
        inst.map(ha)([t[_[_]]] => (ft: FunctorK[t], ta: t[A]) => ft.mapK(ta)(f))
    }

  implicit def functorKConst11[T]: FunctorK[K11.Const[T]] =
    new FunctorK[K11.Const[T]] {
      def mapK[A[_], B[_]](t: T)(f: A ~> B): T = t
    }

  inline def derived[F[_[_]]](implicit gen: K11.Generic[F]): FunctorK[F] =
    functorKGen(K11.mkInstances[FunctorK, F](gen))
}

trait Bifunctor[F[_, _]] {
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]
}

object Bifunctor {
  inline def apply[F[_, _]](implicit bf: Bifunctor[F]): Bifunctor[F] = bf

  def map[S[_, _], A, B](f: A => B)(fsa: Fix[S, A])(implicit bs: Bifunctor[S]): Fix[S, B] =
    Fix(bs.bimap(fsa.unfix)(f, map(f)))

  implicit def bifunctorPair: Bifunctor[Tuple2] =
    new Bifunctor[Tuple2] {
      def bimap[A, B, C, D](fab: (A, B))(f: A => C, g: B => D): (C, D) =
        (f(fab._1), g(fab._2))
    }

  implicit def bifunctorEither: Bifunctor[Either] =
    new Bifunctor[Either] {
      def bimap[A, B, C, D](fab: Either[A, B])(f: A => C, g: B => D): Either[C, D] =
        fab match {
          case Left(a) => Left(f(a))
          case Right(b) => Right(g(b))
        }
    }

  implicit def bifunctorGen[F[_, _]](implicit inst: => K2.Instances[Bifunctor, F]): Bifunctor[F] =
    new Bifunctor[F] {
      def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
        inst.map(fab)([t[_, _]] => (bft: Bifunctor[t], tab: t[A, B]) => bft.bimap(tab)(f, g))
    }

  implicit def bifunctorFirst: Bifunctor[K2.Id1] = new Bifunctor[K2.Id1] {
    def bimap[A, B, C, D](a: A)(f: A => C, g: B => D): C = f(a)
  }

  implicit def bifunctorSecond: Bifunctor[K2.Id2] = new Bifunctor[K2.Id2] {
    def bimap[A, B, C, D](b: B)(f: A => C, g: B => D): D = g(b)
  }

  implicit def bifunctorConst[T]: Bifunctor[K2.Const[T]] = new Bifunctor[K2.Const[T]] {
    def bimap[A, B, C, D](t: T)(f: A => C, g: B => D): T = t
  }

  inline def derived[F[_, _]](implicit gen: K2.Generic[F]): Bifunctor[F] =
    bifunctorGen(K2.mkInstances[Bifunctor, F](gen))
}

trait Case[F, A, B] extends (A => B)

trait Data[F, T, R] {
  def gmapQ(t: T): List[R]
}

object Data extends Data0 {
  def apply[F, T, R](implicit dt: Data[F, T, R]): Data[F, T, R] = dt

  type DFR[F, R] = [t] =>> Data[F, t, R]

  implicit def dataGen[F, T, R](implicit inst: => K0.ProductInstances[DFR[F, R], T]): Data[F, T, R] =
    mkData[F, T, R](t => inst.foldLeft[DFR[F, R], T, List[R]](t)(List.empty[R])(
      [t] => (acc: List[R], dt: Data[F, t, R], t: t) => Continue(dt.gmapQ(t) reverse_::: acc)
    ).reverse)

  implicit def dataGenC[F, T, R](implicit inst: => K0.CoproductInstances[DFR[F, R], T]): Data[F, T, R] =
    mkData[F, T, R](t => inst.fold[DFR[F, R], T, List[R]](t)(
      [t] => (dt: Data[F, t, R], t: t) => dt.gmapQ(t)
    ))

  inline def derived[F, T, R](implicit gen: K0.ProductGeneric[T]): Data[F, T, R] =
    dataGen(K0.mkProductInstances[DFR[F, R], T](gen))

  inline def derived[F, T, R](implicit gen: K0.CoproductGeneric[T]): Data[F, T, R] =
    dataGenC(K0.mkCoproductInstances[DFR[F, R], T](gen))
}

trait Data0 {
  def mkData[F, T, R](f: T => List[R]): Data[F, T, R] =
    new Data[F, T, R] {
      def gmapQ(t: T): List[R] = f(t)
    }

  inline implicit def dataDefault[F, T, R]: Data[F, T, R] = implicit match {
    case fn: Case[F, T, R] => mkData[F, T, R](t => List(fn(t)))
    case _ => mkData[F, T, R](_ => Nil)
  }
}

trait DataT[F, T] {
  type Out
  def gmapT(t: T): Out
}

object DataT {
  type Aux[F, T, Out0] = DataT[F, T] { type Out = Out0 }

  def apply[F, T](implicit dtt: DataT[F, T]): Aux[F, T, dtt.Out] = dtt

  type DF[F] = [t] =>> Aux[F, t, t]

  implicit def dataTGen[F, T](implicit inst: => K0.Instances[DF[F], T]): Aux[F, T, T] =
    mkDataT[F, T, T](t => inst.map[DF[F], T](t)(
      [t] => (dt: Aux[F, t, t], t: t) => dt.gmapT(t)
    ))

  def mkDataT[F, T, R](f: T => R): Aux[F, T, R] =
    new DataT[F, T] {
      type Out = R
      def gmapT(t: T): R = f(t)
    }

  inline implicit def dataTDefault[F, T, R]: Aux[F, T, R] = implicit match {
    case fn: Case[F, T, R] => mkDataT[F, T, R](fn)
    case ev: (T <:< R) => mkDataT[F, T, R](ev)
  }

  inline def derived[F, T](implicit gen: K0.Generic[T]): DataT[F, T] =
    dataTGen(K0.mkInstances[DF[F], T](gen))
}

trait Empty[T] {
  def empty: T
}

object Empty {
  def apply[T](implicit et: Empty[T]): Empty[T] = et

  def mkEmpty[T](t: T): Empty[T] =
    new Empty[T] {
      def empty = t
    }

  implicit def emptyUnit: Empty[Unit] = mkEmpty(())
  implicit def emptyInt: Empty[Int] = mkEmpty(0)
  implicit def emptyString: Empty[String] = mkEmpty("")
  implicit def emptyBoolean: Empty[Boolean] = mkEmpty(false)

  implicit def emptyGen[A](implicit inst: => K0.ProductInstances[Empty, A]): Empty[A] =
    mkEmpty(inst.construct([a] => (ma: Empty[a]) => ma.empty))

  inline implicit def emptyGenC[A](implicit gen: K0.CoproductGeneric[A]): Empty[A] =
    mkEmpty(K0.summonFirst[Empty, gen.MirroredElemTypes, A].empty)

  inline def derived[A](implicit gen: K0.Generic[A]): Empty[A] =
    inline gen match {
      case p: K0.ProductGeneric[A]   => emptyGen(K0.mkProductInstances[Empty, A](p))
      case c: K0.CoproductGeneric[A] => emptyGenC(c)
    }
}

trait EmptyK[F[_]] {
  def empty[A]: F[A]
}

object EmptyK {
  def apply[F[_]](implicit ef: EmptyK[F]): EmptyK[F] = ef

  def mkEmptyK[F[_]](f: [a] => () => F[a]): EmptyK[F] =
    new EmptyK[F] {
      def empty[A] = f[A]()
    }

  implicit def emptyKGen[A[_]](implicit inst: => K1.ProductInstances[EmptyK, A]): EmptyK[A] =
    mkEmptyK([t] => () => inst.construct([f[_]] => (ef: EmptyK[f]) => ef.empty[t]))

  inline implicit def emptyKGenC[A[_]](implicit gen: K1.CoproductGeneric[A]): EmptyK[A] =
    mkEmptyK[A]([t] => () => K1.summonFirst[EmptyK, gen.MirroredElemTypes, A].empty[t])

  inline def derived[A[_]](implicit gen: K1.Generic[A]): EmptyK[A] =
    inline gen match {
      case p: K1.ProductGeneric[A]   => emptyKGen(K1.mkProductInstances[EmptyK, A](p))
      case c: K1.CoproductGeneric[A] => emptyKGenC(c)
    }
}

trait Alt1[F[_[_]], G[_[_]], T[_]] {
  def fold[A](f: F[T] => A)(g: G[T] => A): A
}

object Alt1 {
  type Of[F[_[_]], G[_[_]]] = [t[_]] =>> Alt1[F, G, t]

  class Alt1F[F[_[_]], G[_[_]], T[_]](ft: F[T]) extends Alt1[F, G, T] {
    def fold[A](f: F[T] => A)(g: G[T] => A): A = f(ft)
  }

  class Alt1G[F[_[_]], G[_[_]], T[_]](gt: G[T]) extends Alt1[F, G, T] {
    def fold[A](f: F[T] => A)(g: G[T] => A): A = g(gt)
  }

  inline implicit def apply[F[_[_]], G[_[_]], T[_]]: Alt1[F, G, T] = implicit match {
    case ft: F[T] => new Alt1F(ft)
    case gt: G[T] => new Alt1G(gt)
  }
}

trait Pure[F[_]] {
  def pure[A](a: A): F[A]
}

object Pure {
  def apply[F[_]](implicit ef: Pure[F]): Pure[F] = ef

  def mkPure[F[_]](f: [a] => a => F[a]): Pure[F] =
    new Pure[F] {
      def pure[A](a: A) = f(a)
    }

  implicit def pureId: Pure[Id] = mkPure([T] => (t: T) => t)

  implicit def pureGen[A[_]](implicit inst: K1.ProductInstances[Alt1.Of[Pure, EmptyK], A]): Pure[A] =
    mkPure[A]([t] => (a: t) => inst.construct([f[_]] => (af: Alt1.Of[Pure, EmptyK][f]) => af.fold[f[t]](_.pure(a))(_.empty[t])))

  inline implicit def pureGenC[A[_]](implicit gen: K1.CoproductGeneric[A]): Pure[A] =
    mkPure[A]([t] => (a: t) => K1.summonFirst[Pure, gen.MirroredElemTypes, A].pure(a))

  inline def derived[A[_]](implicit gen: K1.Generic[A]): Pure[A] =
    inline gen match {
      case p: K1.ProductGeneric[A]   => pureGen(K1.mkProductInstances[Alt1.Of[Pure, EmptyK], A](p))
      case c: K1.CoproductGeneric[A] => pureGenC(c)
    }
}

trait Show[T] {
  def show(t: T): String
}

object Show {
  inline def apply[T](implicit st: Show[T]): Show[T] = st

  def mkShow[T](f: T => String): Show[T] =
    new Show[T] {
      def show(t: T): String = f(t)
    }

  implicit val showInt: Show[Int] = (_: Int).toString
  implicit val showString: Show[String] = (s: String) => "\""+s+"\""
  implicit val showBoolean: Show[Boolean] = (_: Boolean).toString

  implicit def showGen[T](implicit inst: => K0.ProductInstances[Show, T], labelling: Labelling[T]): Show[T] =
    new Show[T] {
      def show(t: T): String = {
        if(labelling.elemLabels.isEmpty) labelling.label
        else {
          val elems: List[String] = inst.foldLeft(t)(List.empty[String])(
            [t] => (acc: List[String], st: Show[t], t: t) => Continue(st.show(t) :: acc)
          )
          labelling.elemLabels.zip(elems.reverse).map((k, v) => s"$k: $v").mkString(s"${labelling.label}(", ", ", ")")
        }
      }
    }

  implicit def showGenC[T](implicit inst: => K0.CoproductInstances[Show, T]): Show[T] =
    new Show[T] {
      def show(t: T): String = inst.fold(t)([t] => (st: Show[t], t: t) => st.show(t))
    }

  inline def derived[A](implicit gen: K0.Generic[A]): Show[A] =
    K0.derive(gen, inst => showGen given (inst, Labelling(gen)), showGenC)
}

trait Read[T] {
  def read(s: String): Option[(T, String)]
}

object Read {
  inline def apply[T](implicit rt: Read[T]): Read[T] = rt

  import scala.util.matching.Regex
  import scala.util.Try

  def head(s: String, r: Regex): Option[(String, String)] =
    s.trim match {
      case r(hd, tl) => Some((hd, tl))
      case _ => None
    }

  def readPrimitive[T](r: Regex, f: String => Option[T]): Read[T] =
    (s: String) =>
      for {
        (hd, tl) <- head(s, r)
        p        <- f(hd)
      } yield (p, tl)


  implicit val readInt: Read[Int] = readPrimitive("""(-?\d*)(.*)""".r, s => Try(s.toInt).toOption)
  implicit val readString: Read[String] = (s: String) => head(s, """\"(.*)\"(.*)""".r)
  implicit val readBoolean: Read[Boolean] = readPrimitive("""(true|false)(.*)""".r, s => Try(s.toBoolean).toOption)

  implicit def readGen[T](implicit inst: => K0.ProductInstances[Read, T], labelling: Labelling[T]): Read[T] =
    new Read[T] {
      def read(s: String): Option[(T, String)] = {
        def readUnit(s: String): Option[(T, String)] = {
          inst.unfold[Read, T, Unit](())(
            [t] => (u: Unit, rt: Read[t]) => ((), None)
          )._2.map(t => (t, s))
        }

        def readElems(s: String): Option[(T, String)] = {
          type Acc = (String, Seq[String], Boolean)
          inst.unfold[Read, T, Acc]((s, labelling.elemLabels, true))(
            [t] => (acc: Acc, rt: Read[t]) => {
              val (s, labels, first) = acc
              (for {
                (_, tl0) <- if(first) Some(("", s)) else head(s, "(,)(.*)".r)
                (_, tl1) <- head(tl0, s"(${labels.head}):(.*)".r)
                (t, tl2) <- rt.read(tl1)
                } yield (t, tl2)) match {
                  case Some(t, tl2) => ((tl2, labels.tail, false), Some(t))
                  case None => ((s, labels, first), None)
                }
            }
            ) match {
              case (s, None) => None
              case (acc, Some(t)) => Some((t, acc._1))
            }
        }

        if(labelling.elemLabels.isEmpty) {
          for {
            (_, tl0) <- head(s, s"(${labelling.label})(.*)".r)
            (t, tl1) <- readUnit(tl0)
          } yield (t, tl1)
        } else {
          for {
            (_, tl0) <- head(s, s"(${labelling.label})\\((.*)".r)
            (t, tl1) <- readElems(tl0)
            (_, tl2) <- head(tl1, s"(\\))(.*)".r)
          } yield (t, tl2)
        }
      }
    }

  implicit def readGenC[T](implicit inst: => K0.CoproductInstances[Read, T], labelling: Labelling[T]): Read[T] =
    new Read[T] {
      def read(s: String): Option[(T, String)] = {
        labelling.elemLabels.zipWithIndex.iterator.map((p: (String, Int)) => {
          val (label, i) = p
          if(s.trim.startsWith(label)) {
            inst.project[Read, T, String](i)(s)(
              [t] => (s: String, rt: Read[t]) =>
                rt.read(s) match {
                  case Some((t, tl)) => (tl, Some(t))
                  case None => (s, None)
                }
            ) match {
              case (s, None) => None
              case (tl, Some(t)) => Some((t, tl))
            }
          }
          else None
        }).find(_.isDefined).flatten
      }
    }

  inline def derived[A](implicit gen: K0.Generic[A]): Read[A] =
    K0.derive(gen, inst => readGen given (inst, Labelling(gen)), inst => readGenC given (inst, Labelling(gen)))
}

trait Transform[T, U] {
  def apply(t: T): U
}

object Transform {
  def apply[T, U](implicit ttu: Transform[T, U]): Transform[T, U] = ttu

  inline def mkField[K, KT, RT <: NonEmptyTuple, V](rt: RT): Object =
    (inline constValue[K0.IndexOf[K, KT]] match {
      case -1 => summon[Monoid[V]].empty
      case i => rt(i)
    }).asInstanceOf

  inline def mkFieldArray[KU, RU, KT, RT <: NonEmptyTuple](rt: RT): Array[Object] =
    inline erasedValue[(KU, RU)] match {
      case _: (Unit, Unit) => Array()
      case _: (Tuple1[k0], Tuple1[v0]) =>
        Array(
          mkField[k0, KT, RT, v0](rt)
        )
      case _: ((k0, k1), (v0, v1)) =>
        Array(
          mkField[k0, KT, RT, v0](rt),
          mkField[k1, KT, RT, v1](rt)
        )
      case _: ((k0, k1, k2), (v0, v1, v2)) =>
        Array(
          mkField[k0, KT, RT, v0](rt),
          mkField[k1, KT, RT, v1](rt),
          mkField[k2, KT, RT, v2](rt)
        )

      // Add fallback for larger sizes
    }

  inline def mkRecord[KU, RU <: Tuple, KT, RT <: NonEmptyTuple](rt: RT): RU =
    Tuple.fromArray(mkFieldArray[KU, RU, KT, RT](rt)).asInstanceOf

  inline implicit def transformGen[T, U]
    (implicit
      gent: K0.ProductGeneric[T] { type MirroredElemTypes <: NonEmptyTuple },
      genu: K0.ProductGeneric[U] { type MirroredElemTypes <: Tuple }
    ): Transform[T, U] =
      new Transform[T, U] {
        def apply(t: T): U =
          genu.fromRepr(mkRecord[genu.MirroredElemLabels, genu.MirroredElemTypes, gent.MirroredElemLabels, gent.MirroredElemTypes](gent.toRepr(t)))
      }
}

// ADTs

object ADTs {
  case class ISB(i: Int, s: String, b: Boolean)

  case class Box[A](x: A)

  sealed trait OptionInt
  case class SomeInt(value: Int) extends OptionInt
  case object NoneInt extends OptionInt

  sealed trait Opt[+A]
  case class Sm[+A](value: A) extends Opt[A]
  case object Nn extends Opt[Nothing]

  sealed trait CList[+A]
  case class CCons[+A](hd: A, tl: CList[A]) extends CList[A]
  case object CNil extends CList[Nothing]

  case class Order[F[_]](
    item: F[String],
    quantity: F[Int]
  )

  sealed trait OptionD[T] {
    def fold: T = this match {
      case Given(t) => t
      case Default(t) => t
    }
  }
  object OptionD {
    val fold: OptionD ~> Id = [t] => (ot: OptionD[t]) => ot.fold
  }

  case class Given[T](value: T) extends OptionD[T]
  case class Default[T](value: T) extends OptionD[T]

  sealed trait ListF[+A, +R]
  object ListF {
    type List[A] = Fix[ListF, A]
  }
  case class ConsF[+A, +R](hd: A, tl: R) extends ListF[A, R]
  case object NilF extends ListF[Nothing, Nothing]

  case class BI(b: Boolean, i: Int)
}

object ADTsWithDerives {
  case class ISB(i: Int, s: String, b: Boolean) derives Monoid, Eq, Empty, Show, Read

  case class Box[A](x: A) derives Monoid, Eq, Show, Read, Functor, Pure

  sealed trait OptionInt derives Eq, Empty, Show, Read
  case class SomeInt(value: Int) extends OptionInt
  case object NoneInt extends OptionInt

  sealed trait Opt[+A] derives Eq, Show, Read, Functor, EmptyK, Pure
  case class Sm[+A](value: A) extends Opt[A]
  case object Nn extends Opt[Nothing]

  sealed trait CList[+A] derives Eq, Show, Read, Functor, EmptyK
  case class CCons[+A](hd: A, tl: CList[A]) extends CList[A]
  case object CNil extends CList[Nothing]

  case class Order[F[_]](
    item: F[String],
    quantity: F[Int]
  ) derives FunctorK

  sealed trait OptionD[T] {
    def fold: T = this match {
      case Given(t) => t
      case Default(t) => t
    }
  }
  object OptionD {
    val fold: OptionD ~> Id = [t] => (ot: OptionD[t]) => ot.fold
  }

  case class Given[T](value: T) extends OptionD[T]
  case class Default[T](value: T) extends OptionD[T]

  sealed trait ListF[+A, +R] derives Bifunctor
  object ListF {
    type List[A] = Fix[ListF, A]
  }
  case class ConsF[+A, +R](hd: A, tl: R) extends ListF[A, R]
  case object NilF extends ListF[Nothing, Nothing]
}

// Tests

object Size {
  implicit def intCase: Case[Size.type, Int, Int] = identity(_)
  implicit def stringCase: Case[Size.type, String, Int] = _.length
  implicit def booleanCase: Case[Size.type, Boolean, Int] = _ => 1
}

object Inc {
  implicit def intCase: Case[Inc.type, Int, Int] = _+1
  implicit def stringCase: Case[Inc.type, String, String] = _+"!"
  implicit def booleanCase: Case[Inc.type, Boolean, Boolean] = !_
}

object Test extends App {
  {
    import ADTs._

    val v0 = Monoid[ISB]
    assert(v0.empty == ISB(0, "", false))
    assert(v0.combine(ISB(1, "foo", false), ISB(2, "bar", true)) == ISB(3, "foobar", true))

    val v1 = Monoid[Box[Int]]
    assert(v1.empty == Box(0))
    assert(v1.combine(Box(1), Box(2)) == Box(3))

    val v2 = Functor[Box]
    assert(v2.map(Box("foo"))(_.length) == Box(3))

    val v3 = Eq[SomeInt]
    assert(v3.eqv(SomeInt(23), SomeInt(23)))
    assert(!v3.eqv(SomeInt(23), SomeInt(13)))
    val v4 = Eq[NoneInt.type]
    assert(v4.eqv(NoneInt, NoneInt))
    val v5 = Eq[OptionInt]
    assert(v5.eqv(SomeInt(23), SomeInt(23)))
    assert(!v5.eqv(SomeInt(23), SomeInt(13)))
    assert(!v5.eqv(SomeInt(23), NoneInt))

    val v6 = Eq[Sm[Int]]
    assert(v6.eqv(Sm(23), Sm(23)))
    assert(!v6.eqv(Sm(23), Sm(13)))
    val v7 = Eq[Nn.type]
    assert(v7.eqv(Nn, Nn))
    val v8 = Eq[Opt[Int]]
    assert(v8.eqv(Sm(23), Sm(23)))
    assert(!v8.eqv(Sm(23), Sm(13)))
    assert(!v8.eqv(Sm(23), Nn))

    val v9 = Functor[Sm]
    assert(v9.map(Sm("foo"))(_.length) == Sm(3))
    val v10 = Functor[Const[Nn.type]]
    assert(v10.map(Nn)(identity) == Nn)
    val v11 = Functor[Opt]
    assert(v11.map(Sm("foo"))(_.length) == Sm(3))
    assert(v11.map(Nn)(identity) == Nn)

    val v12 = Eq[CNil.type]
    assert(v12.eqv(CNil, CNil))
    val v13 = Eq[CCons[Int]]
    assert(v13.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(2, CCons(3, CNil)))))
    assert(!v13.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(4, CCons(3, CNil)))))
    val v14 = Eq[CList[Int]]
    assert(v14.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(2, CCons(3, CNil)))))
    assert(!v14.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(4, CCons(3, CNil)))))

    val v15 = Functor[Const[CNil.type]]
    assert(v15.map(CNil)(identity) == CNil)
    val v16 = Functor[CCons]
    assert(v16.map(CCons("foo", CCons("quux", CCons("wibble", CNil))))(_.length) == CCons(3, CCons(4, CCons(6, CNil))))
    val v17 = Functor[CList]
    assert(v17.map(CCons("foo", CCons("quux", CCons("wibble", CNil))))(_.length) == CCons(3, CCons(4, CCons(6, CNil))))
    val v18 = Functor[[t] =>> CList[Opt[t]]]
    assert(v18.map(CCons(Sm("foo"), CCons(Nn, CCons(Sm("quux"), CNil))))(_.length) == CCons(Sm(3), CCons(Nn, CCons(Sm(4), CNil))))

    val v19 = FunctorK[Order]
    assert(v19.mapK(Order[OptionD](Given("Epoisse"), Default(10)))(OptionD.fold) == Order[Id]("Epoisse", 10))

    val v20 = Bifunctor[ConsF]
    val v21 = Bifunctor[ListF]
    val v22: ListF.List[String] = Fix(ConsF("foo", Fix(ConsF("quux", Fix(ConsF("wibble", Fix(NilF)))))))
    val v23: ListF.List[Int] = Fix(ConsF(3, Fix(ConsF(4, Fix(ConsF(6, Fix(NilF)))))))
    assert(Bifunctor.map((_: String).length)(v22) == v23)

    val v24 = Data[Size.type, ISB, Int]
    assert(v24.gmapQ(ISB(23, "foo", true)).sum == 27)
    val v25 = Data[Size.type, OptionInt, Int]
    assert(v25.gmapQ(NoneInt).sum == 0)
    assert(v25.gmapQ(SomeInt(23)).sum == 23)
    val v26 = Data[Size.type, CList[String], Int]
    assert(v26.gmapQ(CCons("foo", CCons("quux", CCons("wibble", CNil)))).sum == 13)

    val v27 = DataT[Inc.type, ISB]
    assert(v27.gmapT(ISB(23, "foo", true)) == ISB(24, "foo!", false))
    val v28 = DataT[Inc.type, OptionInt]
    assert(v28.gmapT(NoneInt) == NoneInt)
    assert(v28.gmapT(SomeInt(23)) == SomeInt(24))
    val v29 = DataT[Inc.type, CList[Int]]
    assert(v29.gmapT(CCons(1, CCons(2, CCons(3, CNil)))) == CCons(2, CCons(3, CCons(4, CNil))))

    val v30 = Empty[ISB]
    assert(v30.empty == ISB(0, "", false))

    val v31 = EmptyK[Opt]
    assert(v31.empty[Int] == Nn)
    val v32 = EmptyK[CList]
    assert(v32.empty[Int] == CNil)

    val v33 = Pure[Box]
    assert(v33.pure(23) == Box(23))
    val v34 = Pure[CList]
    assert(v34.pure(23) == CCons(23, CNil))

    val v35 = K0.Generic[ISB]
    val v36 = summonValues[v35.MirroredElemLabels]
    assert(v36 == ("i", "s", "b"))

    val v37 = Show[ISB]
    assert(v37.show(ISB(23, "foo", true)) == """ISB(i: 23, s: "foo", b: true)""")

    val v38 = Show[OptionInt]
    assert(v38.show(SomeInt(23)) == "SomeInt(value: 23)")
    assert(v38.show(NoneInt) == "NoneInt")

    val v39 = Show[Box[Int]]
    assert(v39.show(Box(23)) == "Box(x: 23)")

    val v40 = Show[Opt[Int]]
    assert(v40.show(Sm(23)) == "Sm(value: 23)")
    assert(v40.show(Nn) == "Nn")

    val v41 = Show[CList[Int]]
    assert(v41.show((CCons(1, CCons(2, CCons(3, CNil))))) == "CCons(hd: 1, tl: CCons(hd: 2, tl: CCons(hd: 3, tl: CNil)))")

    val v42 = Show[Order[Id]]
    assert(v42.show(Order[Id]("Epoisse", 10)) == """Order(item: "Epoisse", quantity: 10)""")

    val v43 = Read[ISB]
    assert(v43.read("""ISB(i: 23, s: "foo", b: true)""") == Some((ISB(23, "foo", true), "")))

    val v44 = Read[OptionInt]
    assert(v44.read("SomeInt(value: 23)") == Some((SomeInt(23), "")))
    assert(v44.read("NoneInt") == Some((NoneInt, "")))

    val v45 = Read[Box[Int]]
    assert(v45.read("Box(x: 23)") == Some((Box(23), "")))

    val v46 = Read[Opt[Int]]
    assert(v46.read("Sm(value: 23)") == Some((Sm(23), "")))
    assert(v46.read("Nn") == Some((Nn, "")))

    val v47 = Read[CList[Int]]
    assert(v47.read("CCons(hd: 1, tl: CCons(hd: 2, tl: CCons(hd: 3, tl: CNil)))") == Some((CCons(1, CCons(2, CCons(3, CNil))), "")))

    val v48 = Read[Order[Id]]
    assert(v48.read("""Order(item: "Epoisse", quantity: 10)""") == Some((Order[Id]("Epoisse", 10), "")))

    val v49 = Transform[BI, ISB]
    assert(v49(BI(true, 23)) == ISB(23, "", true))

    val v50 = K0.ProductGeneric[Box[Int]]
    val v51 = v50.toRepr(Box(23))
    val v51a: Tuple1[Int] = v51
    assert(v51 == Tuple1(23))

    val v52 = K0.ProductGeneric[Order[Id]]
    val v53 = v52.toRepr(Order[Id]("Epoisse", 10))
    val v53a: (String, Int) = v53
    assert(v53 == ("Epoisse", 10))
  }

  {
    import ADTsWithDerives._

    val v0 = Monoid[ISB]
    assert(v0.empty == ISB(0, "", false))
    assert(v0.combine(ISB(1, "foo", false), ISB(2, "bar", true)) == ISB(3, "foobar", true))

    val v1 = Monoid[Box[Int]]
    assert(v1.empty == Box(0))
    assert(v1.combine(Box(1), Box(2)) == Box(3))

    val v2 = Functor[Box]
    assert(v2.map(Box("foo"))(_.length) == Box(3))

    val v5 = Eq[OptionInt]
    assert(v5.eqv(SomeInt(23), SomeInt(23)))
    assert(!v5.eqv(SomeInt(23), SomeInt(13)))
    assert(!v5.eqv(SomeInt(23), NoneInt))

    val v8 = Eq[Opt[Int]]
    assert(v8.eqv(Sm(23), Sm(23)))
    assert(!v8.eqv(Sm(23), Sm(13)))
    assert(!v8.eqv(Sm(23), Nn))

    val v11 = Functor[Opt]
    assert(v11.map(Sm("foo"))(_.length) == Sm(3))
    assert(v11.map(Nn)(identity) == Nn)

    val v14 = Eq[CList[Int]]
    assert(v14.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(2, CCons(3, CNil)))))
    assert(!v14.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(4, CCons(3, CNil)))))

    val v17 = Functor[CList]
    assert(v17.map(CCons("foo", CCons("quux", CCons("wibble", CNil))))(_.length) == CCons(3, CCons(4, CCons(6, CNil))))

    val v19 = FunctorK[Order]
    assert(v19.mapK(Order[OptionD](Given("Epoisse"), Default(10)))(OptionD.fold) == Order[Id]("Epoisse", 10))

    val v21 = Bifunctor[ListF]
    val v22: ListF.List[String] = Fix(ConsF("foo", Fix(ConsF("quux", Fix(ConsF("wibble", Fix(NilF)))))))
    val v23: ListF.List[Int] = Fix(ConsF(3, Fix(ConsF(4, Fix(ConsF(6, Fix(NilF)))))))
    assert(Bifunctor.map((_: String).length)(v22) == v23)

    val v30 = Empty[ISB]
    assert(v30.empty == ISB(0, "", false))

    val v31 = EmptyK[Opt]
    assert(v31.empty[Int] == Nn)
    val v32 = EmptyK[CList]
    assert(v32.empty[Int] == CNil)

    val v33 = Pure[Box]
    assert(v33.pure(23) == Box(23))
    val v34 = Pure[CList]
    assert(v34.pure(23) == CCons(23, CNil))

    val v37 = Show[ISB]
    assert(v37.show(ISB(23, "foo", true)) == """ISB(i: 23, s: "foo", b: true)""")

    val v38 = Show[OptionInt]
    assert(v38.show(SomeInt(23)) == "SomeInt(value: 23)")
    assert(v38.show(NoneInt) == "NoneInt")

    val v39 = Show[Box[Int]]
    assert(v39.show(Box(23)) == "Box(x: 23)")

    val v40 = Show[Opt[Int]]
    assert(v40.show(Sm(23)) == "Sm(value: 23)")
    assert(v40.show(Nn) == "Nn")

    val v41 = Show[CList[Int]]
    assert(v41.show((CCons(1, CCons(2, CCons(3, CNil))))) == "CCons(hd: 1, tl: CCons(hd: 2, tl: CCons(hd: 3, tl: CNil)))")

    val v43 = Read[ISB]
    assert(v43.read("""ISB(i: 23, s: "foo", b: true)""") == Some((ISB(23, "foo", true), "")))

    val v44 = Read[OptionInt]
    assert(v44.read("SomeInt(value: 23)") == Some((SomeInt(23), "")))
    assert(v44.read("NoneInt") == Some((NoneInt, "")))

    val v45 = Read[Box[Int]]
    assert(v45.read("Box(x: 23)") == Some((Box(23), "")))

    val v46 = Read[Opt[Int]]
    assert(v46.read("Sm(value: 23)") == Some((Sm(23), "")))
    assert(v46.read("Nn") == Some((Nn, "")))

    val v47 = Read[CList[Int]]
    assert(v47.read("CCons(hd: 1, tl: CCons(hd: 2, tl: CCons(hd: 3, tl: CNil)))") == Some((CCons(1, CCons(2, CCons(3, CNil))), "")))
  }
}
