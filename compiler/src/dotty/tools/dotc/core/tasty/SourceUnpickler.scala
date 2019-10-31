package dotty.tools.dotc.core.tasty

import java.nio.charset.StandardCharsets.UTF_8

class SourceUnpickler(reader: TastyReader) {
  import reader._

  private[tasty] lazy val source: String = {
    val len = readInt()
    val bytes = readBytes(len)
    new String(bytes, UTF_8)
  }

}
