package dotty.tools.languageserver.compiler

import org.eclipse.lsp4j._

// All case classes in this file should have zero-parameters secondary
// constructors to allow Gson to reflectively create instances on
// deserialization without relying on sun.misc.Unsafe.

/** The response to a `compiler/typechecked` request. */
case class TypecheckedResult(replacement: TextEdit) {
  def this() = this(null)
}

// Would make sense to also include the range of the macro in there.
case class PublishWebviewParams(uri: String, title: String, body: String) {
  def this() = this(null, null, null)
}
