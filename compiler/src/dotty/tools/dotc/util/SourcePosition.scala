package dotty.tools
package dotc
package util

import printing.{Showable, Printer}
import printing.Texts._
import Spans.{Span, NoSpan}
import scala.annotation.internal.sharable

/** A source position is comprised of a span and a source file */
case class SourcePosition(source: SourceFile, span: Span, outer: SourcePosition = NoSourcePosition)
extends interfaces.SourcePosition with Showable {

  private def repToLine(rep: Int): Int =
    if span.isLine then rep
    else if source.exists then source.offsetToLine(rep)
    else -1

  private def repToOffset(rep: Int): Int =
    if span.isOffset then rep
    else if source.exists then source.lineToOffset(rep)
    else -1

  /** Is `that` a source position contained in this source position ?
   *  `outer` is not taken into account. */
  def contains(that: SourcePosition): Boolean =
    this.source == that.source && this.span.contains(that.span)

  def exists: Boolean = span.exists

  def lineContent: String = source.lineContent(point)

  def point: Int = span.point

  def line: Int = repToLine(point)

  /** Extracts the lines from the underlying source file as `Array[Char]`*/
  def linesSlice: Array[Char] =
    if source.exists then
      val from = source.nextLine(repToOffset(start))
      val to   = source.nextLine(repToOffset(end))
      source.content.slice(from, to)
    else Array()

  /** The lines of the position */
  def lines: Range = {
    val startOffset = repToLine(start)
    val endOffset = repToLine(end - 1) // -1 to drop a line if no chars in it form part of the position
    if (startOffset >= endOffset) line to line
    else startOffset to endOffset
  }

  def lineOffsets: List[Int] =
    lines.toList.map(source.lineToOffset(_))

  def beforeAndAfterPoint: (List[Int], List[Int]) =
    lineOffsets.partition(_ <= point)

  def column: Int = if (span.isOffset && source.exists) source.column(point) else -1

  def start: Int = span.start
  def startLine: Int = repToLine(start)
  def startColumn: Int = if (span.isOffset && source.exists) source.column(start) else -1
  def startColumnPadding: String = source.startColumnPadding(start)

  def end: Int = span.end
  def endLine: Int = source.repToLine(end)
  def endColumn: Int = if (span.isOffset && source.exists) source.column(end) else -1

  def withOuter(outer: SourcePosition): SourcePosition = SourcePosition(source, span, outer)
  def withSpan(range: Span) = SourcePosition(source, range, outer)

  def startPos: SourcePosition = withSpan(span.startPos)
  def endPos  : SourcePosition = withSpan(span.endPos)
  def focus   : SourcePosition = withSpan(span.focus)
  def toSynthetic: SourcePosition = withSpan(span.toSynthetic)

  def outermost: SourcePosition =
    if outer == null || outer == NoSourcePosition then this else outer.outermost

  /** Inner most position that is contained within the `outermost` position.
   *  Most precise position that that comes from the call site.
   */
  def nonInlined: SourcePosition = {
    val om = outermost
    def rec(self: SourcePosition): SourcePosition =
      if om.contains(self) then self else rec(self.outer)
    rec(this)
  }


  override def toString: String =
    s"${if (source.exists) source.file.toString else "(no source)"}:$span"

  def toText(printer: Printer): Text = printer.toText(this)
}

/** A sentinel for a non-existing source position */
@sharable object NoSourcePosition extends SourcePosition(NoSource, NoSpan, null) {
  override def toString: String = "?"
  override def withOuter(outer: SourcePosition): SourcePosition = outer
}
