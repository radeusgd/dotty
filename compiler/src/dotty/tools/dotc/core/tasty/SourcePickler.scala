package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Comments.{Comment, CommentsContext, ContextDocstrings}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.tasty.TastyBuffer.{Addr, NoAddr}

import java.nio.charset.Charset
import java.util.Base64
import java.nio.charset.StandardCharsets.UTF_8

class SourcePickler(pickler: TastyPickler)(implicit ctx: Context) {
  private val buf = new TastyBuffer(5000)
  pickler.newSection("Source", buf)

  def pickleSource(): Unit = {
    val contents = ctx.source.content.mkString
    val bytes = contents.getBytes(UTF_8)
    buf.writeInt(bytes.length)
    buf.writeBytes(bytes, bytes.length)
  }

}
