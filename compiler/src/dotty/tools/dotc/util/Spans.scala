package dotty.tools.dotc
package util
import language.implicitConversions

/** The offsets part of a full position, consisting of 2 or 3 entries:
 *    - tag  :  whether the start/end/point stores offset or line numbers
 *    - start:  the start offset of the span, in characters from start of file
 *    - end  :  the end offset of the span
 *    - point:  if given, the offset where a single `^` would be logically placed
 *
 *  Spans are encoded according to the following format in little endian:
 *  Tag  : unsigned 2 Bits
 *  Start: unsigned 25 Bits (works for source files up to 64M)
 *  End: unsigned 25 Bits
 *  Point: unsigned 12 Bits relative to start
 *  NoSpan encoded as -1L (this is a normally invalid span because point would lie beyond end).
 */
object Spans {

  private final val TagBits             = 2
  private final val TagMask             = (1L << TagBits) - 1
  private final val StartEndBits        = 25
  private final val StartShift          = TagBits
  private final val EndShift            = TagBits + StartEndBits
  private final val PointShift          = TagBits + 2 * StartEndBits
  private final val StartEndMask        = (1L << StartEndBits) - 1
  private final val SyntheticPointDelta = (1 << (64 - StartEndBits * 2 - TagBits)) - 1

  /** Tag to indicate whether the span stores offset or line numbers */
  private final val OffsetTag = 0
  private final val LineTag   = 1
  private final val NoSpanTag = 2

  /** The maximal representable offset in a span */
  final val MaxOffset = StartEndMask.toInt

  /** Convert offset `x` to an integer by sign extending the original
   *  field of `StartEndBits` width.
   */
  def offsetToInt(x: Int): Int =
    x << (32 - StartEndBits) >> (32 - StartEndBits)

  /** A span indicates a range between a start offset and an end offset.
   *  Spans can be synthetic or source-derived. A source-derived span
   *  has in addition a point lies somewhere between start and end. The point
   *  is roughly where the ^ would go if an error was diagnosed at that position.
   *  All quantities are encoded opaquely in a Long.
   */
  class Span(val coords: Long) extends AnyVal {

    /** Is this span different from NoSpan? */
    def exists: Boolean = tag == NoSpanTag

    /** Is this a line span, i.e. start/end/point represent lines ? */
    def isLine: Boolean = tag == LineTag

    /** Is this an offset span, i.e. start/end/point represent offsets ? */
    def isOffset: Boolean = tag == OffsetTag

    def tag: Int = (coords & TagMask).toInt

    /** The start of this span. */
    def start: Int = {
      assert(exists)
      ((coords >>> StartShift) & StartEndMask).toInt
    }

    /** The end of this span */
    def end: Int = {
      assert(exists)
      ((coords >>> EndShift) & StartEndMask).toInt
    }

    /** The point of this span, returns start for synthetic spans */
    def point: Int = {
      assert(exists)
      val poff = pointDelta
      if (poff == SyntheticPointDelta) start else start + poff
    }

    /** The difference between point and start in this span */
    def pointDelta: Int =
      (coords >>> PointShift).toInt

    def orElse(that: Span): Span =
      if (this.exists) this else that

    /** The union of two spans. This is the least range that encloses
     *  both spans. It is always a synthetic span.
     */
    def union(that: Span): Span =
      if (!this.exists) that
      else if (!that.exists) this
      else Span(this.start min that.start, this.end max that.end, this.point, isLine)

    /** Does the range of this span contain the one of that span? */
    def contains(that: Span): Boolean =
      !that.exists || exists && (start <= that.start && end >= that.end)

    /** Does the range of this span overlap with the range of that span at more than a single point? */
    def overlaps(that: Span): Boolean = {
      def containsInner(span: Span, offset: Int) = span.start < offset && offset < span.end
      exists && that.exists && (
         containsInner(this, that.start)
      || containsInner(this, that.end)
      || containsInner(that, this.start)
      || containsInner(that, this.end)
      )
    }

    /** Is this span synthetic? */
    def isSynthetic: Boolean = pointDelta == SyntheticPointDelta

    /** Is this span source-derived? */
    def isSourceDerived: Boolean = !isSynthetic

    /** Is this a zero-extent span? */
    def isZeroExtent: Boolean = start == end

     /** A span where all components are shifted by a given `offset`
     *  relative to this span.
     */
    def shift(offset: Int): Span =
      if (exists) fromOffsets(start + offset, end + offset, pointDelta, isLine)
      else this

    /** The zero-extent span with start and end at the point of this span */
    def focus: Span = if (exists) Span(point, isLine) else NoSpan

    /** The zero-extent span with start and end at the start of this span */
    def startPos: Span = if (exists) Span(start, isLine) else NoSpan

    /** The zero-extent span with start and end at the end of this span */
    def endPos: Span = if (exists) Span(end, isLine) else NoSpan

    /** A copy of this span with a different start */
    def withStart(start: Int): Span =
      if exists then
        val point = if (isSynthetic) SyntheticPointDelta else this.point - start
        fromOffsets(start, this.end, point, isLine)
      else this

    /** A copy of this span with a different end */
    def withEnd(end: Int): Span =
      if (exists) fromOffsets(this.start, end, pointDelta, isLine)
      else this

    /** A copy of this span with a different point */
    def withPoint(point: Int): Span =
      if (exists) fromOffsets(this.start, this.end, point - this.start, isLine)
      else this

    /** A synthetic copy of this span */
    def toSynthetic: Span = if (isSynthetic) this else Span(start, end, isLine)

    override def toString: String = {
      val (left, right) = if (isSynthetic) ("<", ">") else ("[", "]")
      if (exists)
        s"$left$start..${if (point == start) "" else s"$point.."}$end$right"
      else
        s"${left}no position${right}"
    }

    def ==(that: Span): Boolean = this.coords == that.coords
    def !=(that: Span): Boolean = this.coords != that.coords
  }

  private def fromOffsets(start: Int, end: Int, pointDelta: Int, isLine: Boolean) =
    //assert(start <= end || start == 1 && end == 0, s"$start..$end")
    new Span(
      (if isLine then LineTag else OffsetTag).toLong |
      ((start & StartEndMask).toLong << StartShift) |
      ((end & StartEndMask).toLong << EndShift) |
      (pointDelta.toLong << PointShift))

  /** A synthetic span with given start and end */
  def Span(start: Int, end: Int, isLine: Boolean): Span =
    fromOffsets(start, end, SyntheticPointDelta, isLine)

  def Span(start: Int, end: Int): Span = Span(start, end, isLine = false)

  /** A source-derived span with given start, end, and point delta */
  def Span(start: Int, end: Int, point: Int, isLine: Boolean): Span = {
    val pointDelta = (point - start) max 0
    fromOffsets(start, end, if (pointDelta >= SyntheticPointDelta) 0 else pointDelta, isLine)
  }

  def Span(start: Int, end: Int, point: Int): Span = Span(start, end, point, isLine = false)

  /** A synthetic zero-extent span that starts and ends at given `start`. */
  def Span(start: Int, isLine: Boolean): Span = Span(start, start, isLine)

  def Span(start: Int): Span = Span(start, isLine = false)

  /** A sentinel for a non-existing span */
  val NoSpan: Span = new Span(NoSpanTag.toLong)

  /** The coordinate of a symbol. This is either an index or
   *  a zero-range span.
   */
  class Coord(val encoding: Int) extends AnyVal {
    def isIndex: Boolean = encoding > 0
    def isSpan: Boolean = encoding <= 0
    def toIndex: Int = {
      assert(isIndex)
      encoding - 1
    }
    def toSpan: Span = {
      assert(isSpan)
      if (this == NoCoord) NoSpan else Span(-1 - encoding, isLine = false)
    }
  }

  /** An index coordinate */
  implicit def indexCoord(n: Int): Coord = new Coord(n + 1)
  implicit def spanCoord(span: Span): Coord =
    if (span.exists) new Coord(-(span.point + 1))
    else NoCoord

  /** A sentinel for a missing coordinate */
  val NoCoord: Coord = new Coord(0)
}
