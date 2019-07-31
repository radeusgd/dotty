package dotty

import scala.collection.immutable.IArrayOps

object DottyPredef {

  @forceInline final def assert(assertion: => Boolean, message: => Any): Unit = {
    if (!assertion)
      assertFail(message)
  }

  @forceInline final def assert(assertion: => Boolean): Unit = {
    if (!assertion)
      assertFail()
  }

  def assertFail(): Unit = throw new java.lang.AssertionError("assertion failed")
  def assertFail(message: => Any): Unit = throw new java.lang.AssertionError("assertion failed: " + message)

  @forceInline final def implicitly[T](implicit ev: T): T = ev

  @forceInline def locally[T](body: => T): T = body

  /**
   * Retrieve the single value of a type with a unique inhabitant.
   *
   * @example {{{
   * object Foo
   * val foo = valueOf[Foo.type]
   * // foo is Foo.type = Foo
   *
   * val bar = valueOf[23]
   * // bar is 23.type = 23
   * }}}
   * @group utilities
   */
  inline def valueOf[T]: T = implicit match {
    case ev: ValueOf[T] => ev.value
  }

  inline def the[T] given (x: T): x.type = x

  // FIXME make IArray arrayops be used for length and apply
  implicit def genericIArrayOps[T](xs: IArray[T]): IArrayOps[T] = (xs match {
    case x: Array[AnyRef]  => refIArrayOps[AnyRef](x.asInstanceOf[IArray[AnyRef]])
    case x: Array[Boolean] => booleanIArrayOps(x.asInstanceOf[IArray[Boolean]])
    case x: Array[Byte]    => byteIArrayOps(x.asInstanceOf[IArray[Byte]])
    case x: Array[Char]    => charIArrayOps(x.asInstanceOf[IArray[Char]])
    case x: Array[Double]  => doubleIArrayOps(x.asInstanceOf[IArray[Double]])
    case x: Array[Float]   => floatIArrayOps(x.asInstanceOf[IArray[Float]])
    case x: Array[Int]     => intIArrayOps(x.asInstanceOf[IArray[Int]])
    case x: Array[Long]    => longIArrayOps(x.asInstanceOf[IArray[Long]])
    case x: Array[Short]   => shortIArrayOps(x.asInstanceOf[IArray[Short]])
    case x: Array[Unit]    => unitIArrayOps(x.asInstanceOf[IArray[Unit]])
    case null              => null
  }).asInstanceOf[IArrayOps[T]]

  implicit def booleanIArrayOps(xs: IArray[Boolean]): IArrayOps.ofBoolean   = new IArrayOps.ofBoolean(xs)
  implicit def byteIArrayOps(xs: IArray[Byte]): IArrayOps.ofByte            = new IArrayOps.ofByte(xs)
  implicit def charIArrayOps(xs: IArray[Char]): IArrayOps.ofChar            = new IArrayOps.ofChar(xs)
  implicit def doubleIArrayOps(xs: IArray[Double]): IArrayOps.ofDouble      = new IArrayOps.ofDouble(xs)
  implicit def floatIArrayOps(xs: IArray[Float]): IArrayOps.ofFloat         = new IArrayOps.ofFloat(xs)
  implicit def intIArrayOps(xs: IArray[Int]): IArrayOps.ofInt               = new IArrayOps.ofInt(xs)
  implicit def longIArrayOps(xs: IArray[Long]): IArrayOps.ofLong            = new IArrayOps.ofLong(xs)
  implicit def refIArrayOps[T <: AnyRef](xs: IArray[T]): IArrayOps.ofRef[T] = new IArrayOps.ofRef[T](xs)
  implicit def shortIArrayOps(xs: IArray[Short]): IArrayOps.ofShort         = new IArrayOps.ofShort(xs)
  implicit def unitIArrayOps(xs: IArray[Unit]): IArrayOps.ofUnit            = new IArrayOps.ofUnit(xs)
}
