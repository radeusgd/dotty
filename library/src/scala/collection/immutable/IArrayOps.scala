// Copied from https://github.com/scala/scala/tree/939450b68541f2eb6c32658b0a470ffd488c67b8

// TODO factor out common code with scala.collection.immutable.IArrayOps and scala.collection.mutable.ArrayOps

/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

import mutable.ArrayBuilder
import mutable.Builder

import scala.reflect.ClassTag
import parallel.mutable.ParArray

/** This class serves as a wrapper for `Array`s with all the operations found in
  *  indexed sequences. Where needed, instances of arrays are implicitly converted
  *  into this class.
  *
  *  The difference between this class and `WrappedIArray` is that calling transformer
  *  methods such as `filter` and `map` will yield an array, whereas a `WrappedIArray`
  *  will remain a `WrappedIArray`.
  *
  *  @since 2.8
  *
  *  @tparam T   type of the elements contained in this array.
  *
  *  @define Coll `Array`
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
sealed trait IArrayOps[T] extends Any with IArrayLike[T, IArray[T]] /*with CustomParallelizable[T, ParArray[T]]*/ {

  private def elementClass: Class[_] =
    repr.getClass.getComponentType

  override def copyToArray[U >: T](xs: Array[U], start: Int, len: Int): Unit = {
    val l = len min repr.length min (xs.length - start)
    if (l > 0) Array.copy(repr.asInstanceOf[Array[T]], 0, xs, start, l)
  }

  override def slice(from: Int, until: Int): IArray[T] = {
    val reprVal = repr
    val lo = math.max(from, 0)
    val hi = math.min(math.max(until, 0), reprVal.length)
    val size = math.max(hi - lo, 0)
    val result = java.lang.reflect.Array.newInstance(elementClass, size)
    if (size > 0) {
      Array.copy(reprVal.asInstanceOf[Array[T]], lo, result, 0, size)
    }
    result.asInstanceOf[IArray[T]]
  }

  override def toArray[U >: T : ClassTag]: Array[U] = {
    val thatElementClass = implicitly[ClassTag[U]].runtimeClass
    if (elementClass eq thatElementClass)
      repr.asInstanceOf[Array[U]]
    else
      super.toArray[U]
  }

  def :+[B >: T: ClassTag](elem: B): IArray[B] = {
    val currentLength = repr.length
    val result = new Array[B](currentLength + 1)
    Array.copy(repr.asInstanceOf[Array[T]], 0, result, 0, currentLength)
    result(currentLength) = elem
    result.asInstanceOf[IArray[B]]
  }

  def +:[B >: T: ClassTag](elem: B): IArray[B] = {
    val currentLength = repr.length
    val result = new Array[B](currentLength + 1)
    result(0) = elem
    Array.copy(repr.asInstanceOf[Array[T]], 0, result, 1, currentLength)
    result.asInstanceOf[IArray[B]]
  }

//  override def par = ParArray.handoff(repr)

  /** Flattens a two-dimensional array by concatenating all its rows
    *  into a single array.
    *
    *  @tparam U        Type of row elements.
    *  @param asTrav    A function that converts elements of this array to rows - arrays of type `U`.
    *  @return          An array obtained by concatenating rows of this array.
    */
  def flatten[U](implicit asTrav: T => scala.collection.Traversable[U], m: ClassTag[U]): IArray[U] = {
    val b = Array.newBuilder[U]
    b.sizeHint(this.asInstanceOf[Array[T]].map{case is: scala.collection.IndexedSeq[_] => is.size case _ => 0}.sum)
    for (xs <- this)
      b ++= asTrav(xs)
    b.result().asInstanceOf[IArray[U]]
  }

  /** Transposes a two dimensional array.
    *
    *  @tparam U       Type of row elements.
    *  @param asArray  A function that converts elements of this array to rows - arrays of type `U`.
    *  @return         An array obtained by replacing elements of this arrays with rows the represent.
    */
  def transpose[U](implicit asArray: T => Array[U]): IArray[Array[U]] = {
    val bb: Builder[Array[U], Array[Array[U]]] = Array.newBuilder(ClassTag[Array[U]](elementClass))
    if (isEmpty) bb.result().asInstanceOf[IArray[Array[U]]]
    else {
      def mkRowBuilder() = Array.newBuilder(ClassTag[U](elementClass.getComponentType))
      val bs = asArray(head) map (_ => mkRowBuilder())
      for (xs <- this) {
        var i = 0
        for (x <- asArray(xs)) {
          bs(i) += x
          i += 1
        }
      }
      for (b <- bs) bb += b.result()
      bb.result().asInstanceOf[IArray[Array[U]]]
    }
  }

  /** Converts an array of pairs into an array of first elements and an array of second elements.
    *
    *  @tparam T1    the type of the first half of the element pairs
    *  @tparam T2    the type of the second half of the element pairs
    *  @param asPair an implicit conversion which asserts that the element type
    *                of this Array is a pair.
    *  @param ct1    a class tag for T1 type parameter that is required to create an instance
    *                of Array[T1]
    *  @param ct2    a class tag for T2 type parameter that is required to create an instance
    *                of Array[T2]
    *  @return       a pair of Arrays, containing, respectively, the first and second half
    *                of each element pair of this Array.
    */
  // implementation NOTE: ct1 and ct2 can't be written as context bounds because desugared
  // implicits are put in front of asPair parameter that is supposed to guide type inference
  def unzip[T1, T2](implicit asPair: T => (T1, T2), ct1: ClassTag[T1], ct2: ClassTag[T2]): (Array[T1], Array[T2]) = {
    val a1 = new Array[T1](length)
    val a2 = new Array[T2](length)
    var i = 0
    while (i < length) {
      val e = asPair(apply(i))
      a1(i) = e._1
      a2(i) = e._2
      i += 1
    }
    (a1, a2)
  }

  /** Converts an array of triples into three arrays, one containing the elements from each position of the triple.
    *
    *  @tparam T1      the type of the first of three elements in the triple
    *  @tparam T2      the type of the second of three elements in the triple
    *  @tparam T3      the type of the third of three elements in the triple
    *  @param asTriple an implicit conversion which asserts that the element type
    *                  of this Array is a triple.
    *  @param ct1    a class tag for T1 type parameter that is required to create an instance
    *                of Array[T1]
    *  @param ct2    a class tag for T2 type parameter that is required to create an instance
    *                of Array[T2]
    *  @param ct3    a class tag for T3 type parameter that is required to create an instance
    *                of Array[T3]
    *  @return         a triple of Arrays, containing, respectively, the first, second, and third
    *                  elements from each element triple of this Array.
    */
  // implementation NOTE: ct1, ct2, ct3 can't be written as context bounds because desugared
  // implicits are put in front of asPair parameter that is supposed to guide type inference
  def unzip3[T1, T2, T3](implicit asTriple: T => (T1, T2, T3), ct1: ClassTag[T1], ct2: ClassTag[T2],
                         ct3: ClassTag[T3]): (Array[T1], Array[T2], Array[T3]) = {
    val a1 = new Array[T1](length)
    val a2 = new Array[T2](length)
    val a3 = new Array[T3](length)
    var i = 0
    while (i < length) {
      val e = asTriple(apply(i))
      a1(i) = e._1
      a2(i) = e._2
      a3(i) = e._3
      i += 1
    }
    (a1, a2, a3)
  }

  def seq = thisCollection
}

/**
  * A companion object for `IArrayOps`.
  *
  * @since 2.8
  */
object IArrayOps {

  /** A class of `IArrayOps` for arrays containing reference types. */
  final class ofRef[T <: AnyRef](override val repr: IArray[T]) extends AnyVal with IArrayOps[T] with IArrayLike[T, IArray[T]] {

    override protected[this] def thisCollection: WrappedIArray[T] = new WrappedIArray.ofRef[T](repr)
    override protected[this] def toCollection(repr: IArray[T]): WrappedIArray[T] = new WrappedIArray.ofRef[T](repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofRef[T]()(ClassTag[T](repr.getClass.getComponentType)).asInstanceOf[Builder[T, opaques.IArray[T]]]

    def length: Int = repr.length
    def apply(index: Int): T = repr(index)
  }

  /** A subclass of `IArrayOps` for arrays containing `Byte`s. */
  final class ofByte(override val repr: IArray[Byte]) extends AnyVal with IArrayOps[Byte] with IArrayLike[Byte, IArray[Byte]] {

    override protected[this] def thisCollection: WrappedIArray[Byte] = new WrappedIArray.ofByte(repr)
    override protected[this] def toCollection(repr: IArray[Byte]): WrappedIArray[Byte] = new WrappedIArray.ofByte(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofByte().asInstanceOf[Builder[Byte, opaques.IArray[Byte]]]

    def length: Int = repr.length
    def apply(index: Int): Byte = repr(index)
  }

  /** A subclass of `IArrayOps` for arrays containing `Short`s. */
  final class ofShort(override val repr: IArray[Short]) extends AnyVal with IArrayOps[Short] with IArrayLike[Short, IArray[Short]] {

    override protected[this] def thisCollection: WrappedIArray[Short] = new WrappedIArray.ofShort(repr)
    override protected[this] def toCollection(repr: IArray[Short]): WrappedIArray[Short] = new WrappedIArray.ofShort(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofShort().asInstanceOf[Builder[Short, opaques.IArray[Short]]]

    def length: Int = repr.length
    def apply(index: Int): Short = repr(index)
  }

  /** A subclass of `IArrayOps` for arrays containing `Char`s. */
  final class ofChar(override val repr: IArray[Char]) extends AnyVal with IArrayOps[Char] with IArrayLike[Char, IArray[Char]] {

    override protected[this] def thisCollection: WrappedIArray[Char] = new WrappedIArray.ofChar(repr)
    override protected[this] def toCollection(repr: IArray[Char]): WrappedIArray[Char] = new WrappedIArray.ofChar(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofChar().asInstanceOf[Builder[Char, opaques.IArray[Char]]]

    def length: Int = repr.length
    def apply(index: Int): Char = repr(index)
  }

  /** A subclass of `IArrayOps` for arrays containing `Int`s. */
  final class ofInt(override val repr: IArray[Int]) extends AnyVal with IArrayOps[Int] with IArrayLike[Int, IArray[Int]] {

    override protected[this] def thisCollection: WrappedIArray[Int] = new WrappedIArray.ofInt(repr)
    override protected[this] def toCollection(repr: IArray[Int]): WrappedIArray[Int] = new WrappedIArray.ofInt(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofInt().asInstanceOf[Builder[Int, opaques.IArray[Int]]]

    def length: Int = repr.length
    def apply(index: Int): Int = repr(index)
  }

  /** A subclass of `IArrayOps` for arrays containing `Long`s. */
  final class ofLong(override val repr: IArray[Long]) extends AnyVal with IArrayOps[Long] with IArrayLike[Long, IArray[Long]] {

    override protected[this] def thisCollection: WrappedIArray[Long] = new WrappedIArray.ofLong(repr)
    override protected[this] def toCollection(repr: IArray[Long]): WrappedIArray[Long] = new WrappedIArray.ofLong(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofLong().asInstanceOf[Builder[Long, opaques.IArray[Long]]]

    def length: Int = repr.length
    def apply(index: Int): Long = repr(index)
  }

  /** A subclass of `IArrayOps` for arrays containing `Float`s. */
  final class ofFloat(override val repr: IArray[Float]) extends AnyVal with IArrayOps[Float] with IArrayLike[Float, IArray[Float]] {

    override protected[this] def thisCollection: WrappedIArray[Float] = new WrappedIArray.ofFloat(repr)
    override protected[this] def toCollection(repr: IArray[Float]): WrappedIArray[Float] = new WrappedIArray.ofFloat(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofFloat().asInstanceOf[Builder[Float, opaques.IArray[Float]]]

    def length: Int = repr.length
    def apply(index: Int): Float = repr(index)
  }

  /** A subclass of `IArrayOps` for arrays containing `Double`s. */
  final class ofDouble(override val repr: IArray[Double]) extends AnyVal with IArrayOps[Double] with IArrayLike[Double, IArray[Double]] {

    override protected[this] def thisCollection: WrappedIArray[Double] = new WrappedIArray.ofDouble(repr)
    override protected[this] def toCollection(repr: IArray[Double]): WrappedIArray[Double] = new WrappedIArray.ofDouble(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofDouble().asInstanceOf[Builder[Double, opaques.IArray[Double]]]

    def length: Int = repr.length
    def apply(index: Int): Double = repr(index)
  }

  /** A subclass of `IArrayOps` for arrays containing `Boolean`s. */
  final class ofBoolean(override val repr: IArray[Boolean]) extends AnyVal with IArrayOps[Boolean] with IArrayLike[Boolean, IArray[Boolean]] {

    override protected[this] def thisCollection: WrappedIArray[Boolean] = new WrappedIArray.ofBoolean(repr)
    override protected[this] def toCollection(repr: IArray[Boolean]): WrappedIArray[Boolean] = new WrappedIArray.ofBoolean(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofBoolean().asInstanceOf[Builder[Boolean, opaques.IArray[Boolean]]]

    def length: Int = repr.length
    def apply(index: Int): Boolean = repr(index)
  }

  /** A subclass of `IArrayOps` for arrays of `Unit` types. */
  final class ofUnit(override val repr: IArray[Unit]) extends AnyVal with IArrayOps[Unit] with IArrayLike[Unit, IArray[Unit]] {

    override protected[this] def thisCollection: WrappedIArray[Unit] = new WrappedIArray.ofUnit(repr)
    override protected[this] def toCollection(repr: IArray[Unit]): WrappedIArray[Unit] = new WrappedIArray.ofUnit(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofUnit().asInstanceOf[Builder[Unit, opaques.IArray[Unit]]]

    def length: Int = repr.length
    def apply(index: Int): Unit = repr(index)
  }
}
