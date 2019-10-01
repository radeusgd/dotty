import * as vscode from 'vscode'
import * as path from 'path'
import { CancellationTokenSource, ProgressLocation, Location, Uri } from 'vscode'
import { CompilerTypecheckedResult, CompilerTypecheckedRequest } from './protocol'
import { BaseLanguageClient } from 'vscode-languageclient'
import { Disposable } from 'vscode-jsonrpc'

export const compilerTypecheckedKey = "dotty.compiler.typechecked"
export const compilerExecuteMacroCommandsKey = "dotty.compiler.executeMacroCommands"

export class CompilerProvider implements Disposable {
  private disposables: Disposable[] = []

  private provider = new MacroResultsProvider()

  constructor(
    readonly client: BaseLanguageClient,
    readonly documentSelector: vscode.DocumentSelector) {

    this.disposables.push(
      vscode.window.registerTreeDataProvider("macroCommandsResult", this.provider),
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
      vscode.commands.registerTextEditorCommand(compilerExecuteMacroCommandsKey, (editor, _edit, macroEdits) => {
        let document = editor.document // FIXME: support edits to other docs
        const edit = new vscode.WorkspaceEdit()
        const textEdits = client.protocol2CodeConverter.asTextEdits(macroEdits)
        console.log("textEdits: ", textEdits)
        const locs: Location[] = textEdits.map(e => new Location(document.uri, e.range))
        this.provider.setResults(locs)

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

class MacroResultsProvider implements vscode.TreeDataProvider<string | Location> {
  private _onDidChangeTreeData: vscode.EventEmitter<any> = new vscode.EventEmitter<any>();
  readonly onDidChangeTreeData: vscode.Event<any> = this._onDidChangeTreeData.event;

  refresh(): any {
    this._onDidChangeTreeData.fire();
  }

  private results: Map<string, Location[]> = new Map()

  public setResults(locs: Location[]) {
    this.results.clear()
    // No groupBy in JS :(
    locs.forEach(loc => {
      const key = loc.uri.toString()
      var slot = this.results.get(key)
      if (slot === undefined) {
        slot = []
        this.results.set(key, slot)
      }
      slot.push(loc)
    })
    this.refresh()
  }

  public getChildren(element?: string | Location): string[] | Location[] {
    if (!element)
      return Array.from(this.results.keys())
    if (typeof element == "string")
      return this.results.get(element) || []
    return []
  }

  public getTreeItem(element: string | Location): vscode.TreeItem {
    if (typeof element == "string")
      return new vscode.TreeItem(
        Uri.parse(element).path.split("/").slice(-1)[0],
        vscode.TreeItemCollapsibleState.Expanded)

    // TODO: display code instead
    return new vscode.TreeItem(element.uri.toString())
  }
}
