import scala.quoted.{_, given}
import scala.quoted.autolift.given

import scala.language.implicitConversions

case class Xml(parts: String, args: List[Any])

object XmlQuote {

  implicit object SCOps {
    inline def (inline ctx: StringContext) xml (args: => Any*): Xml =
      ${XmlQuote.impl(ctx, 'args)}
  }

  def impl(receiver: StringContext, args: Expr[Seq[Any]])(given QuoteContext): Expr[Xml] = {
    val string = receiver.parts.mkString("??")
    '{new Xml(${string}, $args.toList)}
  }
}
