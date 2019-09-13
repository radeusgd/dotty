import * as vscode from 'vscode'
import * as path from 'path'
import { CancellationTokenSource, ProgressLocation } from 'vscode'
import { TastyDecompileRequest, TastyDecompileResult,
         asTastyDecompileParams, } from './protocol'
import { BaseLanguageClient } from 'vscode-languageclient'
import { Disposable } from 'vscode-jsonrpc'

export const compilerTypecheckedKey = "dotty.compiler.typechecked"

export class CompilerProvider implements Disposable {
  private disposables: Disposable[] = []

  constructor(
    readonly client: BaseLanguageClient,
    readonly documentSelector: vscode.DocumentSelector) {
    this.disposables.push(
      vscode.commands.registerTextEditorCommand(compilerTypecheckedKey, (editor, edit) => {
        let document = editor.document.uri 
       // check using documentSelector ...
        let position = editor.position
        this.client
          .sendRequest(CompilerTypecheckedRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position))
          .then(response => {
            return null
          })
      })
    )
  }

  dispose(): void {
    this.disposables.forEach(d => d.dispose())
    this.disposables = []
  }
}
