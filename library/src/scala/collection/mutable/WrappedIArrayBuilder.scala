// Copied from https://github.com/scala/scala/tree/939450b68541f2eb6c32658b0a470ffd488c67b8

// TODO factor out common code with WrappedArrayBuilder and WrappedIArrayBuilder


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
package mutable

import immutable.WrappedIArray

import scala.reflect.ClassTag

/** A builder class for arrays.
  *
  *  This builder can be reused.
  *
  *  @tparam A   type of elements that can be added to this builder.
  *  @param tag  class tag for objects of type `A`.
  *
  *  @since 2.8
  */
class WrappedIArrayBuilder[A](tag: ClassTag[A]) extends ReusableBuilder[A, WrappedIArray[A]] {

  @deprecated("use tag instead", "2.10.0")
  val manifest: ClassTag[A] = tag

  private var elems: WrappedIArray[A] = _
  private var capacity: Int = 0
  private var size: Int = 0

  private def mkArray(size: Int): WrappedIArray[A] = {
    val runtimeClass = tag.runtimeClass
    val newelems = runtimeClass match {
      case java.lang.Byte.TYPE      => new WrappedIArray.ofByte(new Array[Byte](size).asInstanceOf[IArray[Byte]]).asInstanceOf[WrappedIArray[A]]
      case java.lang.Short.TYPE     => new WrappedIArray.ofShort(new Array[Short](size).asInstanceOf[IArray[Short]]).asInstanceOf[WrappedIArray[A]]
      case java.lang.Character.TYPE => new WrappedIArray.ofChar(new Array[Char](size).asInstanceOf[IArray[Char]]).asInstanceOf[WrappedIArray[A]]
      case java.lang.Integer.TYPE   => new WrappedIArray.ofInt(new Array[Int](size).asInstanceOf[IArray[Int]]).asInstanceOf[WrappedIArray[A]]
      case java.lang.Long.TYPE      => new WrappedIArray.ofLong(new Array[Long](size).asInstanceOf[IArray[Long]]).asInstanceOf[WrappedIArray[A]]
      case java.lang.Float.TYPE     => new WrappedIArray.ofFloat(new Array[Float](size).asInstanceOf[IArray[Float]]).asInstanceOf[WrappedIArray[A]]
      case java.lang.Double.TYPE    => new WrappedIArray.ofDouble(new Array[Double](size).asInstanceOf[IArray[Double]]).asInstanceOf[WrappedIArray[A]]
      case java.lang.Boolean.TYPE   => new WrappedIArray.ofBoolean(new Array[Boolean](size).asInstanceOf[IArray[Boolean]]).asInstanceOf[WrappedIArray[A]]
      case java.lang.Void.TYPE      => new WrappedIArray.ofUnit(new Array[Unit](size).asInstanceOf[IArray[Unit]]).asInstanceOf[WrappedIArray[A]]
      case _                        => new WrappedIArray.ofRef[A with AnyRef](tag.newArray(size).asInstanceOf[IArray[A with AnyRef]]).asInstanceOf[WrappedIArray[A]]
    }
    if (this.size > 0) Array.copy(elems.array.asInstanceOf[Array[A]], 0, newelems.array.asInstanceOf[Array[A]], 0, this.size)
    newelems
  }

  private def resize(size: Int): Unit = {
    elems = mkArray(size)
    capacity = size
  }

  override def sizeHint(size: Int): Unit = {
    if (capacity < size) resize(size)
  }

  private def ensureSize(size: Int): Unit = {
    if (capacity < size) {
      var newsize = if (capacity == 0) 16 else capacity * 2
      while (newsize < size) newsize *= 2
      resize(newsize)
    }
  }

  def +=(elem: A): this.type = {
    ensureSize(size + 1)
    val array = elems.array.asInstanceOf[Array[A]]
    array(size) = elem
    size += 1
    this
  }

  def clear(): Unit = { size = 0 }

  def result() = {
    if (capacity != 0 && capacity == size) {
      capacity = 0
      elems
    }
    else mkArray(size)
  }

  // todo: add ++=
}
