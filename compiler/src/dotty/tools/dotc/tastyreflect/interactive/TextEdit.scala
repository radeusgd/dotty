package dotty.tools.dotc
package tastyreflect
package interactive

case class TextEdit(pos: util.SourcePosition, newText: String)
type WorkspaceEdit = List[TextEdit]

case class Result(pos: util.SourcePosition, label: String)

case class Webview(title: String, body: String)
