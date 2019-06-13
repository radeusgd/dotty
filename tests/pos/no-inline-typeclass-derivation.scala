import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.mutable.WrappedArray
import scala.compiletime._
import scala.deriving._

object Test {
  enum E16[T] derives Eq {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
    case C7(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T)
    case C8(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T)
    case C9(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T)
    case C10(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T)
    case C11(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T)
    case C12(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T)
    case C13(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T)
    case C14(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T, x14: T)
    case C15(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T, x14: T, x15: T)
    case C16(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T, x14: T, x15: T, x16: T)
  }

  sealed trait CompleteOr[T]
  case class Complete[T](t: T) extends CompleteOr[T]
  case class Continue[T](t: T) extends CompleteOr[T]

  object Complete {
    def apply[T](c: Boolean)(t: T)(f: T): CompleteOr[T] =
      if(c) Complete(t)
      else Continue(f)
  }

  abstract class ErasedInstances[FT] {
    def erasedMap(x: Any)(f: (Any, Any) => Any): Any
  }

  final class ErasedProductInstances[FT](val mirror: Mirror.Product, is0: => Array[Any]) extends ErasedInstances[FT] {
    lazy val is = is0

    def toProduct(x: Any): Product = x.asInstanceOf[Product]

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

    implicit def summonProductInstances[F[_], T]
      given (m: ProductGeneric[T])
      given (i: SummonAsList[F, m.MirroredElemTypes]): ErasedProductInstances[F[T]] = {
        new ErasedProductInstances[F[T]](m, i.instances.toArray)
      }

    implicit def summonCoproductInstances[F[_], T, R <: Tuple]
      given (m: CoproductGeneric[T])
      given (i: SummonAsList[F, m.MirroredElemTypes]): ErasedCoproductInstances[F[T]] = {
        assert(i.instances.nonEmpty)
        new ErasedCoproductInstances[F[T]](m, i.instances.toArray)
      }

    class SummonAsList[F[_], T](val instances: List[Any])

    object SummonAsList {
      implicit def caseNil[F[_]]: SummonAsList[F, Unit] =
        new SummonAsList[F, Unit](Nil)

      implicit def caseCons[F[_], H, T <: Tuple](implicit h: F[H], t: SummonAsList[F, T]): SummonAsList[F, H *: T] =
        new SummonAsList[F, H *: T](h :: t.instances)
    }

    implicit object Ops {
      def (gen: ProductGeneric[Obj]) toRepr [Obj] (o: Obj): gen.MirroredElemTypes = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes]
      def (gen: ProductGeneric[Obj]) fromRepr [Obj] (r: gen.MirroredElemTypes): Obj = gen.fromProduct(r.asInstanceOf).asInstanceOf[Obj]
      def (inst: Instances[F, T]) map [F[_], T] (x: T)(f: [t] => (F[t], t) => t): T =
        inst.erasedMap(x)(f.asInstanceOf).asInstanceOf
      def (inst: ProductInstances[F, T]) construct [F[_], T] (f: [t] => F[t] => t): T =
        inst.erasedConstruct(f.asInstanceOf).asInstanceOf
      def (inst: ProductInstances[F, T]) unfold [F[_], T, Acc] (i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
        inst.erasedUnfold(i)(f.asInstanceOf).asInstanceOf
      def (inst: ProductInstances[F, T]) map2 [F[_], T] (x: T, y: T)(f: [t] => (F[t], t, t) => t): T =
        inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
      def (inst: ProductInstances[F, T]) foldLeft [F[_], T, Acc] (x: T)(i: Acc)(f: [t] => (Acc, F[t], t) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
      def (inst: ProductInstances[F, T]) foldLeft2 [F[_], T, Acc] (x: T, y: T)(i: Acc)(f: [t] => (Acc, F[t], t, t) => CompleteOr[Acc]): Acc =
        inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf
      def (inst: CoproductInstances[F, T]) project [F[_], T, Acc] (p: Int)(i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
        inst.erasedProject(p)(i)(f.asInstanceOf).asInstanceOf
      def (inst: CoproductInstances[F, T]) fold [F[_], T, R] (x: T)(f: [t] => (F[t], t) => R): R =
        inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
      def (inst: CoproductInstances[F, T]) fold2 [F[_], T, R] (x: T, y: T)(a: => R)(f: [t] => (F[t], t, t) => R): R =
        inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    }

    type ProductGenericR[O, R] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes = R }
    type CoproductGenericR[O, R] = Mirror.Sum { type MirroredType = O ; type MirroredElemTypes = R }
  }

  trait Eq[A] {
    def eqv(x: A, y: A): Boolean
  }

  object Eq {
    def apply[A](implicit ea: Eq[A]): Eq[A] = ea

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

    case class Wrap[X](unwrap: X) // An opaque type would also do the trick...

    implicit def eqGen[A](implicit inst: => K0.ProductInstances[Eq, A]): Wrap[Eq[A]] =
      Wrap(new Eq[A] {
        def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean)(
          [t] => (acc: Boolean, eqt: Eq[t], t0: t, t1: t) => Complete(!eqt.eqv(t0, t1))(false)(true)
        )
      })

    implicit def eqGenC[A](implicit inst: => K0.CoproductInstances[Eq, A]): Wrap[Eq[A]] =
      Wrap(new Eq[A] {
        def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false)(
          [t] => (eqt: Eq[t], t0: t, t1: t) => eqt.eqv(t0, t1)
        )
      })

    implicit def derived[A](implicit w: Wrap[Eq[A]]): Eq[A] = w.unwrap
  }
}
