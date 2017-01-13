import java.net.URI
import java.nio.file._
import java.util.function._
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j
import org.eclipse.lsp4j.jsonrpc.{CancelChecker, CompletableFutures}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services._

import java.util.{List => jList}
import java.util.ArrayList

import dotty.tools.dotc._
import dotty.tools.dotc.util._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.SymDenotations._
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.tasty._
import dotty.tools.dotc.{ Main => DottyMain }
import dotty.tools.dotc.interfaces
import dotty.tools.dotc.reporting._
import dotty.tools.dotc.reporting.diagnostic._

import scala.collection._
import scala.collection.JavaConverters._

// Not needed with Scala 2.12
import scala.compat.java8.FunctionConverters._

import dotty.tools.FatalError
import dotty.tools.io._
import scala.io.Codec
import dotty.tools.dotc.util.SourceFile
import java.io._

import Flags._, Symbols._, Names._
import core.Decorators._

import ast.Trees._


class ScalaLanguageServer extends LanguageServer with LanguageClientAware { thisServer =>
  import ast.tpd._

  import ScalaLanguageServer._
  var driver: ServerDriver = _

  var publishDiagnostics: PublishDiagnosticsParams => Unit = _
  var rewrites: dotty.tools.dotc.rewrite.Rewrites = _

  val actions = new mutable.LinkedHashMap[org.eclipse.lsp4j.Diagnostic, Command]

  var classPath: String = _
  var target: String = _


  override def connect(client: LanguageClient): Unit = {
    publishDiagnostics = client.publishDiagnostics _
  }

  override def exit(): Unit = {
    println("exit")
    System.exit(0)
  }
  override def shutdown(): CompletableFuture[Object] = {
    println("shutdown")
    CompletableFuture.completedFuture(new Object)
  }

  def computeAsync[R](fun: CancelChecker => R): CompletableFuture[R] =
    CompletableFutures.computeAsync({(cancelToken: CancelChecker) =>
      cancelToken.checkCanceled()
      fun(cancelToken)
    }.asJava)

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = computeAsync { cancelToken =>

    val ensime = scala.io.Source.fromURL("file://" + params.getRootPath + "/.ensime").mkString

    classPath = """:compile-deps (.*)""".r.unanchored.findFirstMatchIn(ensime).map(_.group(1)) match {
      case Some(deps) =>
        """"(.*?)"""".r.unanchored.findAllMatchIn(deps).map(_.group(1)).mkString(":")
      case None =>
        //println("XX#: " + ensime + "###")
        ""
    }
    target = """:targets \("(.*)"\)""".r.unanchored.findFirstMatchIn(ensime).map(_.group(1)).get
    println("classPath: " + classPath)
    println("target: " + target)

    val toolcp = "../library/target/scala-2.11/classes"
    driver = new ServerDriver(List(/*"-Yplain-printer",*//*"-Yprintpos",*/ "-Ylog:frontend", "-Ystop-after:frontend", "-language:Scala2", "-rewrite", "-classpath", toolcp + ":" + target + ":" + classPath))


    val c = new ServerCapabilities
    c.setTextDocumentSync(TextDocumentSyncKind.Full)
    c.setDefinitionProvider(true)
    c.setRenameProvider(true)
    c.setHoverProvider(true)
    c.setCodeActionProvider(true)
    c.setWorkspaceSymbolProvider(true)
    c.setReferencesProvider(true)
    c.setCompletionProvider(new CompletionOptions(
      /* resolveProvider = */ false,
      /* triggerCharacters = */ List(".").asJava))

    new InitializeResult(c)
  }

  override def getTextDocumentService(): TextDocumentService = new TextDocumentService {
    override def codeAction(params: CodeActionParams): CompletableFuture[jList[_ <: Command]] = computeAsync { cancelToken =>
      val l = params.getContext.getDiagnostics.asScala.flatMap{d =>
        actions.get(d)
      }
      l.asJava
    }
    override def codeLens(params: CodeLensParams): CompletableFuture[jList[_ <: CodeLens]] = null
    // FIXME: share code with messages.NotAMember
    override def completion(params: TextDocumentPositionParams): CompletableFuture[CompletionList] = computeAsync { cancelToken =>
      val trees = driver.trees
      val pos = driver.sourcePosition(new URI(params.getTextDocument.getUri), params.getPosition)
      implicit val ctx: Context = driver.ctx

      // Dup with typeOf
      val tree = trees.filter({ case (source, t) =>
        source == pos.source && {
          t.pos.contains(pos.pos)
        }}).head._2
      val paths = ast.NavigateAST.pathTo(pos.pos, tree).asInstanceOf[List[Tree]]

      val boundary = driver.enclosingSym(paths)

      println("paths: " + paths.map(x => (x.show, x.tpe.show)))
      val decls = paths.head match {
        case Select(qual, _) =>
          qual.tpe.accessibleMembers(boundary).filter(!_.symbol.isConstructor)
        case _ =>
          // FIXME: Get all decls in current scope
          boundary.enclosingClass match {
            case csym: ClassSymbol =>
              val classRef = csym.classInfo.typeRef
              println("##boundary: " + boundary)
              val sb = new StringBuffer
              val d = classRef.accessibleMembers(boundary, whyNot = sb).filter(!_.symbol.isConstructor)
              println("WHY NOT: " + sb)
              d
            case _ =>
              Seq()
          }
      }

      val items = decls.map({ decl =>
        val item = new CompletionItem
        item.setLabel(decl.symbol.name.show.toString)
        item.setDetail(decl.info.widenTermRefExpr.show.toString)
        item.setKind(completionItemKind(decl.symbol))
        item
      }).toList

      new CompletionList(/*isIncomplete = */ false, items.asJava)
    }
    override def definition(params: TextDocumentPositionParams): CompletableFuture[jList[_ <: Location]] = computeAsync { cancelToken =>
      val trees = driver.trees
      val pos = driver.sourcePosition(new URI(params.getTextDocument.getUri), params.getPosition)
      val tp = driver.typeOf(trees, pos)
      //println("XXPATHS: " + paths.map(_.show))
      println("Looking for: " + tp)
      if (tp eq NoType)
        List[Location]().asJava
      else {
        val defPos = driver.findDef(trees, tp)
        if (defPos.pos == Positions.NoPosition)
          List[Location]().asJava
        else
          List(location(defPos)).asJava
      }
    }
    override def didChange(params: DidChangeTextDocumentParams): Unit = {
      val document = params.getTextDocument
      val uri = URI.create(document.getUri)
      val path = Paths.get(uri)
      val change = params.getContentChanges.get(0)
      assert(change.getRange == null, "TextDocumentSyncKind.Incremental support is not implemented")
      val text = change.getText

      val diagnostics = new mutable.ArrayBuffer[lsp4j.Diagnostic]
      val tree = driver.run(uri, text, new ServerReporter(thisServer, diagnostics))

      // TODO: Do this for didOpen too ? (So move to driver.run like openFiles)

      publishDiagnostics(new PublishDiagnosticsParams(
        document.getUri,
        java.util.Arrays.asList(diagnostics.toSeq: _*)))
    }
    override def didClose(params: DidCloseTextDocumentParams): Unit = {
      val document = params.getTextDocument
      val uri = URI.create(document.getUri)

      driver.openClasses.remove(uri)
      driver.openFiles.remove(uri)
    }
    override def didOpen(params: DidOpenTextDocumentParams): Unit = {
      val document = params.getTextDocument
      val uri = URI.create(document.getUri)
      val path = Paths.get(uri)
      println("open: " + path)
      val text = params.getTextDocument.getText

      //.setReporter(new ServerReporter)

      val diagnostics = new mutable.ArrayBuffer[lsp4j.Diagnostic]
      val tree = driver.run(uri, text, new ServerReporter(thisServer, diagnostics))

      publishDiagnostics(new PublishDiagnosticsParams(
        document.getUri,
        java.util.Arrays.asList(diagnostics.toSeq: _*)))
    }
    override def didSave(params: DidSaveTextDocumentParams): Unit = {}
    override def documentHighlight(params: TextDocumentPositionParams): CompletableFuture[jList[_ <: DocumentHighlight]] = null
    override def documentSymbol(params: DocumentSymbolParams): CompletableFuture[jList[_ <: SymbolInformation]] = null
    override def hover(params: TextDocumentPositionParams): CompletableFuture[Hover] = computeAsync { cancelToken =>
      implicit val ctx: Context = driver.ctx

      val pos = driver.sourcePosition(new URI(params.getTextDocument.getUri), params.getPosition)
      val tp = driver.typeOf(driver.trees, pos)
      println("hover: " + tp.show)

      val str = tp.widenTermRefExpr.show.toString
      new Hover(List(str).asJava, null)
    }

    override def formatting(params: DocumentFormattingParams): CompletableFuture[jList[_ <: TextEdit]] = null
    override def rangeFormatting(params: DocumentRangeFormattingParams): CompletableFuture[jList[_ <: TextEdit]] = null
    override def onTypeFormatting(params: DocumentOnTypeFormattingParams): CompletableFuture[jList[_ <: TextEdit]] = null

    override def references(params: ReferenceParams): CompletableFuture[jList[_ <: Location]] = computeAsync { cancelToken =>
      val trees = driver.trees
      val pos = driver.sourcePosition(new URI(params.getTextDocument.getUri), params.getPosition)

      val tp = driver.typeOf(trees, pos)
      println("Find all references to " + tp)

      val poss = driver.typeReferences(trees, tp, params.getContext.isIncludeDeclaration)
      val locs = poss.map(location)
      locs.asJava
    }
    override def rename(params: RenameParams): CompletableFuture[WorkspaceEdit] = computeAsync { cancelToken =>
      val trees = driver.trees
      val pos = driver.sourcePosition(new URI(params.getTextDocument.getUri), params.getPosition)

      val tp = driver.typeOf(trees, pos)
      implicit val ctx: Context = driver.ctx
      val sym = tp match {
        case tp: NamedType => tp.symbol
        case _ => NoSymbol
      }

      if (sym eq NoSymbol)
        new WorkspaceEdit()
      else {
        val newName = params.getNewName

        val poss = driver.typeReferences(trees, tp, includeDeclaration = true)

        val changes = poss.groupBy(pos => toUri(pos.source).toString).mapValues(_.map(pos => new TextEdit(nameRange(pos, sym.name.length), newName)).asJava)

        new WorkspaceEdit(changes.asJava)
      }
    }
    override def resolveCodeLens(params: CodeLens): CompletableFuture[CodeLens] = null
    override def resolveCompletionItem(params: CompletionItem): CompletableFuture[CompletionItem] = null
    override def signatureHelp(params: TextDocumentPositionParams): CompletableFuture[SignatureHelp] = null
  }

  override def getWorkspaceService(): WorkspaceService = new WorkspaceService {
    override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = {}
    override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {}
    override def symbol(params: WorkspaceSymbolParams): CompletableFuture[jList[_ <: SymbolInformation]] = computeAsync { cancelToken =>
      val trees = driver.trees
      val syms = driver.symbolInfos(trees, params.getQuery)
      syms.asJava
    }
  }
}

object ScalaLanguageServer {
  import ast.tpd._

  def nameRange(p: SourcePosition, nameLength: Int): Range = {
    val (beginName, endName) =
      if (p.pos.isSynthetic)
        (p.pos.end - nameLength, p.pos.end)
      else
        (p.pos.point, p.pos.point + nameLength)

    new Range(
      new Position(p.source.offsetToLine(beginName), p.source.column(beginName)),
      new Position(p.source.offsetToLine(endName), p.source.column(endName))
    )
  }

  def range(p: SourcePosition): Range =
    new Range(
      new Position(p.startLine, p.startColumn),
      new Position(p.endLine, p.endColumn)
    )

  def toUri(source: SourceFile) = Paths.get(source.file.path).toUri

  def location(p: SourcePosition) =
    new Location(toUri(p.source).toString, range(p))

  def symbolKind(sym: Symbol)(implicit ctx: Context) =
    if (sym.is(Package))
      SymbolKind.Package
    else if (sym.isConstructor)
      SymbolKind.Constructor
    else if (sym.isClass)
      SymbolKind.Class
    else if (sym.is(Mutable))
      SymbolKind.Variable
    else if (sym.is(Method))
      SymbolKind.Method
    else
      SymbolKind.Field

  def completionItemKind(sym: Symbol)(implicit ctx: Context) =
    if (sym.is(Package))
      CompletionItemKind.Module // No CompletionItemKind.Package
    else if (sym.isConstructor)
      CompletionItemKind.Constructor
    else if (sym.isClass)
      CompletionItemKind.Class
    else if (sym.is(Mutable))
      CompletionItemKind.Variable
    else if (sym.is(Method))
      CompletionItemKind.Method
    else
      CompletionItemKind.Field

  def symbolInfo(sourceFile: SourceFile, t: Tree)(implicit ctx: Context) = {
    assert(t.pos.exists)
    val sym = t.symbol
    val s = new SymbolInformation
    s.setName(sym.name.show.toString)
    s.setKind(symbolKind(sym))
    s.setLocation(location(new SourcePosition(sourceFile, t.pos)))
    if (sym.owner.exists && !sym.owner.isEmptyPackage)
      s.setContainerName(sym.owner.name.show.toString)
    s
  }
}
