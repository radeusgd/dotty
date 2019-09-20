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
import dotc.ast.Trees._
import dotc.ast._
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

      val pos = sourcePosition(driver, uri, params.getPosition)

      val untpdTrees = driver.openedUntypedTrees(uri)
      val tpdTrees = driver.openedTrees(uri)

      val path = untpdTrees.find(_.pos.contains(pos)) match {
        case Some(tree) =>
          NavigateAST.pathTo(pos.span, tree.tree, skipZeroExtent = true)
            .collect { case t: untpd.Tree => t }
        case None =>
          Nil
      }
      println("path: " + path)
      val limit = path.indexWhere(_.isInstanceOf[untpd.DefTree | untpd.Block])
      println("limit: " + limit)
      val untpd = path(limit-1).show
      println("untpd: " + untpd)

      /*
      val path = Interactive.pathTo(uriTrees, pos)
      val limit = path.indexWhere(_.isInstanceOf[tpd.DefTree | tpd.Block])
      if (limit == -1 || limit == 0)
        println("path: " + path.map(_.getClass))
      val expr = path(limit-1).show
      */

      val expr = untpd
      TypecheckedResult(expr)
    }
}
