package dotty.tools.languageserver.compiler

import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification

/**
 * A `LanguageClient` that supports compiler-specific notifications.
 */
trait CompilerClient extends LanguageClient {
  @JsonNotification("compiler/publishWebview")
  def publishWebview(webview: PublishWebviewParams): Unit
}

