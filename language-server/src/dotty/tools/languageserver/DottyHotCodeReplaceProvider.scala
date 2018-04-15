package dotty.tools
package languageserver

import com.microsoft.java.debug.core._
import com.microsoft.java.debug.core.adapter._

import java.net.URI
import java.nio.file._
import java.nio.charset.StandardCharsets

import java.util.concurrent.CompletableFuture
import java.util.function._
import java.util.{List => JList}
import java.util.logging.Level
import java.util.logging.Logger

import scala.collection.JavaConverters._

import dotc.core.Symbols.NoSymbol
import dotc.interactive.Interactive

import org.eclipse.lsp4j

import _root_.io.reactivex.Observable

// TODO
class DottyHotCodeReplaceProvider extends IHotCodeReplaceProvider {
  val logger = Logger.getLogger("java-debug")

  override def getEventHub(): Observable[HotCodeReplaceEvent] = Observable.empty

  override def onClassRedefined(consumer: Consumer[JList[String]]): Unit = {}
  override def redefineClasses(): CompletableFuture[JList[String]] =
    CompletableFuture.completedFuture(Nil.asJava)
}
