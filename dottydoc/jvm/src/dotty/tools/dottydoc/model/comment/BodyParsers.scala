package dotty.tools.dottydoc
package model
package comment

object BodyParsers {
  implicit class BodyToHtml(val body: Body) extends AnyVal {
    def toHtml: String = bodyToHtml(body)

    private def inlineToHtml(inl: Inline): String = inl match {
      case Chain(items)     => (items map inlineToHtml).mkString
      case Italic(in)       => s"<i>${inlineToHtml(in)}</i>"
      case Bold(in)         => s"<b>${inlineToHtml(in)}</b>"
      case Underline(in)    => s"<u>${inlineToHtml(in)}</u>"
      case Superscript(in)  => s"<sup>${inlineToHtml(in)}</sup>"
      case Subscript(in)    => s"<sub>${inlineToHtml(in) }</sub>"
      case Link(raw, title) => s"""<a href=$raw target="_blank">${inlineToHtml(title)}</a>"""
      case Monospace(in)    => s"<code>${inlineToHtml(in)}</code>"
      case Text(text)       => text
      case Summary(in)      => inlineToHtml(in)
      case HtmlTag(tag)     => tag
      //TODO: when we have EntityLinks, they should be enabled here too
      //case EntityLink(target, link) => linkToHtml(target, link, hasLinks = true)
    }

    private def bodyToHtml(body: Body): String =
      (body.blocks map blockToHtml).mkString

    private def blockToHtml(block: Block): String = block match {
      case Title(in, 1)  => s"<h1>${inlineToHtml(in)}</h1>"
      case Title(in, 2)  => s"<h2>${inlineToHtml(in)}</h2>"
      case Title(in, 3)  => s"<h3>${inlineToHtml(in)}</h3>"
      case Title(in, _)  => s"<h4>${inlineToHtml(in)}</h4>"
      case Paragraph(in) => s"<p>${inlineToHtml(in)}</p>"
      case Code(data)    => s"""<pre><code class="scala">$data</code></pre>"""
      case UnorderedList(items) =>
        s"<ul>${listItemsToHtml(items)}</ul>"
      case OrderedList(items, listStyle) =>
        s"<ol class=${listStyle}>${listItemsToHtml(items)}</ol>"
      case DefinitionList(items) =>
        s"<dl>${items map { case (t, d) => s"<dt>${inlineToHtml(t)}</dt><dd>${blockToHtml(d)}</dd>" } }</dl>"
      case HorizontalRule() =>
        "<hr/>"
    }

    private def listItemsToHtml(items: Seq[Block]) =
      items.foldLeft(""){ (list, item) =>
        item match {
          case OrderedList(_, _) | UnorderedList(_) =>  // html requires sub ULs to be put into the last LI
            list + s"<li>${blockToHtml(item)}</li>"
          case Paragraph(inline) =>
            list + s"<li>${inlineToHtml(inline)}</li>"  // LIs are blocks, no need to use Ps
          case block =>
            list + s"<li>${blockToHtml(block)}</li>"
        }
    }
  }
}
