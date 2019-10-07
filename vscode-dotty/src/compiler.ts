import * as vscode from 'vscode'
import * as path from 'path'
import * as ls from 'vscode-languageserver-protocol'
import { CancellationTokenSource, ProgressLocation, Location, Uri, TextEdit, DiagnosticRelatedInformation, WebviewPanel } from 'vscode'
import { CompilerTypecheckedResult, CompilerTypecheckedRequest, CompilerPublishWebviewNotification} from './protocol'
import { BaseLanguageClient } from 'vscode-languageclient'
import { Disposable } from 'vscode-jsonrpc'

export const compilerTypecheckedKey = "dotty.compiler.typechecked"
export const compilerExecuteMacroCommandsKey = "dotty.compiler.executeMacroCommands"
export const compilerWebviewKey = "dotty.compiler.webview"

interface Result {
  location: Location
  label: string
}

export class CompilerProvider implements Disposable {
  private disposables: Disposable[] = []

  private provider = new MacroResultsProvider()

  private webviewPanel: WebviewPanel | undefined = undefined

  constructor(
    readonly client: BaseLanguageClient,
    readonly documentSelector: vscode.DocumentSelector) {

    // DiagnosticRelatedInformation is used to represent Results, see DottyLanguageServer#codeLens
    function asResult(information: ls.DiagnosticRelatedInformation): Result {
      return {
        location: client.protocol2CodeConverter.asLocation(information.location),
        label: information.message
      }
    }

    client.onNotification(CompilerPublishWebviewNotification.type, (webview) => {
      if (this.webviewPanel) {
        this.webviewPanel.title = webview.title.toString()
        this.webviewPanel.webview.html = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
</head>
<body>
${webview.body}
</body>
</html>`
      }
    })

    this.disposables.push(
      vscode.window.registerTreeDataProvider("macroCommandsResult", this.provider),
      vscode.commands.registerTextEditorCommand(compilerWebviewKey, (editor, _edit) => {
        if (!this.webviewPanel) {
          this.webviewPanel = vscode.window.createWebviewPanel(
            "dottyCompilerWebview",
            "Macro web view",
            {
              preserveFocus: true,
              viewColumn: vscode.ViewColumn.Beside
            },
            {}
          )
          this.webviewPanel.onDidDispose(() => this.webviewPanel = undefined, this, this.disposables)
        }
      }),
      vscode.commands.registerTextEditorCommand(compilerTypecheckedKey, (editor, _edit) => {
        let document = editor.document
       // check using documentSelector ...
        let position = editor.selection.active
        this.client
          .sendRequest(CompilerTypecheckedRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position))
          .then(response => {
            console.log("response: ", response)
            const edit = new vscode.WorkspaceEdit()
            edit.set(document.uri, [ client.protocol2CodeConverter.asTextEdit(response.replacement) ])
            vscode.workspace.applyEdit(edit)
          })
      }),
      vscode.commands.registerTextEditorCommand(compilerExecuteMacroCommandsKey, (editor, _edit, macroEdits, macroResults) => {
        let document = editor.document // FIXME: support edits to other docs
        const edit = new vscode.WorkspaceEdit()
        const textEdits = client.protocol2CodeConverter.asTextEdits(macroEdits)
        // console.log("textEdits: ", textEdits)
        // const results: Result[] = macroResults.map(asResult)
        // console.log("results: ", results)
        // // this.provider.setResults(results)

        edit.set(document.uri, textEdits)
        vscode.workspace.applyEdit(edit)
      })
    )
  }

  dispose(): void {
    this.disposables.forEach(d => d.dispose())
    this.disposables = []
  }
}

class MacroResultsProvider implements vscode.TreeDataProvider<string | Result> {
  private _onDidChangeTreeData: vscode.EventEmitter<any> = new vscode.EventEmitter<any>();
  readonly onDidChangeTreeData: vscode.Event<any> = this._onDidChangeTreeData.event;

  refresh(): any {
    this._onDidChangeTreeData.fire();
  }

  private resultsMap: Map<string, Result[]> = new Map()

  public setResults(results: Result[]) {
    this.resultsMap.clear()
    // No groupBy in JS :(
    results.forEach(result => {
      const key = result.location.uri.toString()
      var slot = this.resultsMap.get(key)
      if (slot === undefined) {
        slot = []
        this.resultsMap.set(key, slot)
      }
      slot.push(result)
    })
    this.refresh()
  }

  public getChildren(element?: string | Result): string[] | Result[] {
    if (!element)
      return Array.from(this.resultsMap.keys())
    if (typeof element == "string")
      return this.resultsMap.get(element) || []
    return []
  }

  public getTreeItem(element: string | Result): vscode.TreeItem {
    if (typeof element == "string")
      return new vscode.TreeItem(
        Uri.parse(element).path.split("/").slice(-1)[0],
        vscode.TreeItemCollapsibleState.Expanded)

    // TODO: command to go to element.location
    return new vscode.TreeItem(element.label)
  }
}
