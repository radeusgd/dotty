package dotty.tools
package languageserver
package compiler

import java.net.URI
import java.nio.file._
import java.util.concurrent.CompletableFuture

import dotc.core.Contexts._
import dotc.util.Spans._
import dotc.core.tasty.TastyUnpickler.UnpickleException
import dotc.fromtasty.TastyFileUtil
import dotc.util._
import dotc.interactive.Interactive

import org.eclipse.lsp4j.jsonrpc.services._
import org.eclipse.lsp4j._


@JsonSegment("compiler")
trait CompilerService {
  thisServer: DottyLanguageServer =>
  import DottyLanguageServer._

  @JsonRequest
  def typechecked(params: TextDocumentPositionParams): CompletableFuture[TypecheckedResult] =
    computeAsync { cancelChecker =>
      val uri = new URI(params.getTextDocument.getUri)
      val driver = driverFor(uri)
      implicit def ctx: Context = driver.currentCtx

      val uriTrees = driver.openedTrees(uri)
      val pos = sourcePosition(driver, uri, params.getPosition)

      val path = Interactive.pathTo(uriTrees, pos)
      println("head: " + path.headOption)

      TypecheckedResult("")
    }
}
