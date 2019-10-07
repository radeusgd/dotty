package scala.tasty

import scala.quoted.QuoteContext
import scala.tasty.reflect._

class Reflection(private[scala] val internal: CompilerInterface)
    extends Core
    with ConstantOps
    with ContextOps
    with CommentOps
    with FlagsOps
    with IdOps
    with ImplicitsOps
    with ImportSelectorOps
    with QuotedOps
    with PatternOps
    with PositionOps
    with Printers
    with ReportingOps
    with RootPosition
    with SignatureOps
    with StandardDefinitions
    with SymbolOps
    with TreeOps
    with TreeUtils
    with TypeOrBoundsOps { self =>

  def typeOf[T: scala.quoted.Type]: Type =
    implicitly[scala.quoted.Type[T]].unseal.tpe

  def TextEdit(pos: Position, newText: String): TextEdit =
    internal.TextEdit(pos, newText)

  def applyEdit(edit: WorkspaceEdit): Unit = internal.applyEdit(edit)

  def Result(pos: Position, label: String): Result =
    internal.Result(pos, label)

  def setResults(results: List[Result]): Unit = internal.setResults(results)

  def Completion(label: String, description: String, symbols: List[Symbol]): Completion =
    internal.Completion(label, description, symbols)

  def setCompletions(completions: List[Completion]): Unit =
    internal.setCompletions(completions)

  def setReferences(references: List[Position]): Unit =
    internal.setReferences(references)

  def Webview(title: String, body: String): Webview =
    internal.Webview(title, body)
  def setWebview(webview: Webview): Unit =
    internal.setWebview(webview)

  def allTrees(given Context): List[Tree] =
    internal.allTrees
}
