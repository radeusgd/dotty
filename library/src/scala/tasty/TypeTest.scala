package scala.tasty

/** A `TypeTest[S, T]` (where `T <: S`) contains the logic needed to know at runtime if a value of
 *  type `S` can be downcased to `T`.
 *
 *  If a pattern match is performed on a term of type `s: S` that is uncheckable with `s.isInstanceOf[T]` and
 *  the pattern are of the form:
 *    - `t: T`
 *    - `t @ X()` where the `X.unapply` has takes an argument of type `T`
 *  then a given instance of `TypeTest[S, T]` is summoned and used to performed the test.
 *
 *  Note: This is replacemet for `ClassTag.unapply` that can be sound for path dependent types
 */
@scala.annotation.implicitNotFound(msg = "No TypeTest available for [${S}, ${T}]")
trait TypeTest[S, T <: S] extends Serializable {

  def isInstance(x: S): TypeTest.Result[x.type & T]

  /** A TypeTest[S, T] can serve as an extractor that matches only S of type T.
   *
   * The compiler tries to turn unchecked type tests in pattern matches into checked ones
   * by wrapping a `(_: T)` type pattern as `tt(_: T)`, where `ct` is the `TypeTest[S, T]` instance.
   * Type tests necessary before calling other extractors are treated similarly.
   * `SomeExtractor(...)` is turned into `tt(SomeExtractor(...))` if `T` in `SomeExtractor.unapply(x: T)`
   * is uncheckable, but we have an instance of `TypeTest[S, T]`.
   */
  def unapply(x: S): Option[x.type & T] =
    if isInstance(x).asInstanceOf[Boolean] then Some(x.asInstanceOf[x.type & T])
    else None

}

object TypeTest {

  opaque type Result[A] = Boolean

  def success[A](x: A): Result[A] = true

  def failure[A]: Result[A] = false

}
