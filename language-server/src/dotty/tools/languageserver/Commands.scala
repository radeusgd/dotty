package dotty.tools
package languageserver

object Commands {
  final val DEBUG_STARTSESSION = "vscode.dotty.startDebugSession";

  final val RESOLVE_CLASSPATH = "vscode.dotty.resolveClasspath";

  final val RESOLVE_MAINCLASS = "vscode.dotty.resolveMainClass";

  final val BUILD_WORKSPACE = "dotty.workspace.compile";

  final val UPDATE_DEBUG_SETTINGS = "vscode.dotty.updateDebugSettings";
}
