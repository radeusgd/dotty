package scala.tasty.interactive

import java.net.URI

import dotty.tools.dotc.interfaces.SourcePosition

// enum Commands {
//   case WorkspaceEdit(edits: List[(URI, TextEdit)])
// }

type WorkspaceEdit = List[TextEdit]


case class TextEdit(pos: SourcePosition, newText: String)
