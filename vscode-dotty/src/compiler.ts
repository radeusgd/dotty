import * as vscode from 'vscode'
import * as path from 'path'
import { CancellationTokenSource, ProgressLocation } from 'vscode'
import { CompilerTypecheckedResult, CompilerTypecheckedRequest } from './protocol'
import { BaseLanguageClient } from 'vscode-languageclient'
import { Disposable } from 'vscode-jsonrpc'

export const compilerTypecheckedKey = "dotty.compiler.typechecked"
export const compilerExecuteMacroCommandsKey = "dotty.compiler.executeMacroCommands"

export class CompilerProvider implements Disposable {
  private disposables: Disposable[] = []

  constructor(
    readonly client: BaseLanguageClient,
    readonly documentSelector: vscode.DocumentSelector) {
    this.disposables.push(
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
        const edit = new vscode.WorkspaceEdit()
        const textEdits = client.protocol2CodeConverter.asTextEdits(macroEdits)
        console.log("textEdits: ", textEdits)
        let document = editor.document // FIXME: support edits to other docs
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
