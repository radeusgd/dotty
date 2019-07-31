// Copied from https://github.com/scala/scala/tree/939450b68541f2eb6c32658b0a470ffd488c67b8

// TODO factor out common code with scala.collection.immutable.WrappedArray into scala.collection.WrappedArray

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

import scala.reflect.ClassTag
import scala.collection.generic._
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.WrappedIArrayBuilder
import scala.collection.mutable.Builder
import scala.collection.parallel.mutable.ParArray
import scala.util.hashing.MurmurHash3

import java.util.Arrays

/**
  *  A class representing `IArray[T]`.
  *
  *  @tparam T    type of the elements in this wrapped array.
  *
  *  @author  Martin Odersky, Stephane Micheloud
  *  @since 2.8
  *  @define Coll `WrappedArray`
  *  @define coll wrapped array
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
abstract class WrappedIArray[T]
  extends AbstractSeq[T]
    with IndexedSeq[T]
    with IArrayLike[T, WrappedIArray[T]]
//    with CustomParallelizable[T, ParArray[T]]
{

  override protected[this] def thisCollection: WrappedIArray[T] = this
  override protected[this] def toCollection(repr: WrappedIArray[T]): WrappedIArray[T] = repr

  /** The tag of the element type */
  def elemTag: ClassTag[T]

  @deprecated("use elemTag instead", "2.10.0")
  def elemManifest: ClassManifest[T] = ClassManifest.fromClass[T](elemTag.runtimeClass.asInstanceOf[Class[T]])

  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): T

  /** The underlying array */
  def array: IArray[T]

//  override def par = ParArray.handoff(array)

  private def elementClass: Class[_] =
    array.getClass.getComponentType

  override def toArray[U >: T : ClassTag]: Array[U] = {
    val thatElementClass = implicitly[ClassTag[U]].runtimeClass
    if (elementClass eq thatElementClass)
      array.asInstanceOf[Array[U]]
    else
      super.toArray[U]
  }

  override def stringPrefix = "WrappedIArray"

  /** Clones this object, including the underlying Array. */
  override def clone(): WrappedIArray[T] = this

  /** Creates new builder for this collection ==> move to subclasses
    */
  override protected[this] def newBuilder: Builder[T, WrappedIArray[T]] =
    new WrappedIArrayBuilder[T](elemTag)

}

/** A companion object used to create instances of `WrappedIArray`.
  */
object WrappedIArray {
  // This is reused for all calls to empty.
  private val EmptyWrappedIArray  = new ofRef[AnyRef](IArray[AnyRef]() given the[scala.reflect.ClassTag[AnyRef]])
  def empty[T <: AnyRef]: WrappedIArray[T] = EmptyWrappedIArray.asInstanceOf[WrappedIArray[T]]

  // If make is called explicitly we use whatever we're given, even if it's
  // empty.  This may be unnecessary (if WrappedIArray is to honor the collections
  // contract all empty ones must be equal, so discriminating based on the reference
  // equality of an empty array should not come up) but we may as well be
  // conservative since wrapRefArray contributes most of the unnecessary allocations.
  def make[T](x: AnyRef): WrappedIArray[T] = (x match {
    case null              => null
    case x: Array[AnyRef]  => new ofRef[AnyRef](x.asInstanceOf[IArray[AnyRef]])
    case x: Array[Int]     => new ofInt(x.asInstanceOf[IArray[Int]])
    case x: Array[Double]  => new ofDouble(x.asInstanceOf[IArray[Double]])
    case x: Array[Long]    => new ofLong(x.asInstanceOf[IArray[Long]])
    case x: Array[Float]   => new ofFloat(x.asInstanceOf[IArray[Float]])
    case x: Array[Char]    => new ofChar(x.asInstanceOf[IArray[Char]])
    case x: Array[Byte]    => new ofByte(x.asInstanceOf[IArray[Byte]])
    case x: Array[Short]   => new ofShort(x.asInstanceOf[IArray[Short]])
    case x: Array[Boolean] => new ofBoolean(x.asInstanceOf[IArray[Boolean]])
    case x: Array[Unit]    => new ofUnit(x.asInstanceOf[IArray[Unit]])
  }).asInstanceOf[WrappedIArray[T]]

  implicit def canBuildFrom[T](implicit m: ClassTag[T]): CanBuildFrom[WrappedIArray[_], T, WrappedIArray[T]] =
    new CanBuildFrom[WrappedIArray[_], T, WrappedIArray[T]] {
      def apply(from: WrappedIArray[_]): Builder[T, WrappedIArray[T]] =
        ArrayBuilder.make[T]()(m) mapResult WrappedIArray.make[T]
      def apply: Builder[T, WrappedIArray[T]] =
        ArrayBuilder.make[T]()(m) mapResult WrappedIArray.make[T]
    }

  def newBuilder[A]: Builder[A, scala.collection.mutable.IndexedSeq[A]] = new ArrayBuffer

  final class ofRef[T <: AnyRef](val array: IArray[T]) extends WrappedIArray[T] with Serializable {
    lazy val elemTag = ClassTag[T](array.getClass.getComponentType)
    def length: Int = array.length
    def apply(index: Int): T = array(index).asInstanceOf[T]
    override def hashCode = MurmurHash3.wrappedArrayHash(array.asInstanceOf[Array[T]])
  }

  final class ofByte(val array: IArray[Byte]) extends WrappedIArray[Byte] with Serializable {
    def elemTag = ClassTag.Byte
    def length: Int = array.length
    def apply(index: Int): Byte = array(index)
    override def hashCode = MurmurHash3.wrappedBytesHash(array.asInstanceOf[Array[Byte]])
    override def equals(that: Any) = that match {
      case that: ofByte => Arrays.equals(array.asInstanceOf[Array[Byte]], that.array.asInstanceOf[Array[Byte]])
      case _ => super.equals(that)
    }
  }

  final class ofShort(val array: IArray[Short]) extends WrappedIArray[Short] with Serializable {
    def elemTag = ClassTag.Short
    def length: Int = array.length
    def apply(index: Int): Short = array(index)
    override def hashCode = MurmurHash3.wrappedArrayHash(array.asInstanceOf[Array[Short]])
    override def equals(that: Any) = that match {
      case that: ofShort => Arrays.equals(array.asInstanceOf[Array[Short]], that.array.asInstanceOf[Array[Short]])
      case _ => super.equals(that)
    }
  }

  final class ofChar(val array: IArray[Char]) extends WrappedIArray[Char] with Serializable {
    def elemTag = ClassTag.Char
    def length: Int = array.length
    def apply(index: Int): Char = array(index)
    override def hashCode = MurmurHash3.wrappedArrayHash(array.asInstanceOf[Array[Char]])
    override def equals(that: Any) = that match {
      case that: ofChar => Arrays.equals(array.asInstanceOf[Array[Char]], that.array.asInstanceOf[Array[Char]])
      case _ => super.equals(that)
    }
  }

  final class ofInt(val array: IArray[Int]) extends WrappedIArray[Int] with Serializable {
    def elemTag = ClassTag.Int
    def length: Int = array.length
    def apply(index: Int): Int = array(index)
    override def hashCode = MurmurHash3.wrappedArrayHash(array.asInstanceOf[Array[Int]])
    override def equals(that: Any) = that match {
      case that: ofInt => Arrays.equals(array.asInstanceOf[Array[Int]], that.array.asInstanceOf[Array[Int]])
      case _ => super.equals(that)
    }
  }

  final class ofLong(val array: IArray[Long]) extends WrappedIArray[Long] with Serializable {
    def elemTag = ClassTag.Long
    def length: Int = array.length
    def apply(index: Int): Long = array(index)
    override def hashCode = MurmurHash3.wrappedArrayHash(array.asInstanceOf[Array[Long]])
    override def equals(that: Any) = that match {
      case that: ofLong => Arrays.equals(array.asInstanceOf[Array[Long]], that.array.asInstanceOf[Array[Long]])
      case _ => super.equals(that)
    }
  }

  final class ofFloat(val array: IArray[Float]) extends WrappedIArray[Float] with Serializable {
    def elemTag = ClassTag.Float
    def length: Int = array.length
    def apply(index: Int): Float = array(index)
    override def hashCode = MurmurHash3.wrappedArrayHash(array.asInstanceOf[Array[Float]])
    override def equals(that: Any) = that match {
      case that: ofFloat => Arrays.equals(array.asInstanceOf[Array[Float]], that.array.asInstanceOf[Array[Float]])
      case _ => super.equals(that)
    }
  }

  final class ofDouble(val array: IArray[Double]) extends WrappedIArray[Double] with Serializable {
    def elemTag = ClassTag.Double
    def length: Int = array.length
    def apply(index: Int): Double = array(index)
    override def hashCode = MurmurHash3.wrappedArrayHash(array.asInstanceOf[Array[Double]])
    override def equals(that: Any) = that match {
      case that: ofDouble => Arrays.equals(array.asInstanceOf[Array[Double]], that.array.asInstanceOf[Array[Double]])
      case _ => super.equals(that)
    }
  }

  final class ofBoolean(val array: IArray[Boolean]) extends WrappedIArray[Boolean] with Serializable {
    def elemTag = ClassTag.Boolean
    def length: Int = array.length
    def apply(index: Int): Boolean = array(index)
    override def hashCode = MurmurHash3.wrappedArrayHash(array.asInstanceOf[Array[Boolean]])
    override def equals(that: Any) = that match {
      case that: ofBoolean => Arrays.equals(array.asInstanceOf[Array[Boolean]], that.array.asInstanceOf[Array[Boolean]])
      case _ => super.equals(that)
    }
  }

  final class ofUnit(val array: IArray[Unit]) extends WrappedIArray[Unit] with Serializable {
    def elemTag = ClassTag.Unit
    def length: Int = array.length
    def apply(index: Int): Unit = array(index)
    override def hashCode = MurmurHash3.wrappedArrayHash(array.asInstanceOf[Array[Unit]])
    override def equals(that: Any) = that match {
      case that: ofUnit => array.length == that.array.length
      case _ => super.equals(that)
    }
  }
}
