'use strict';

import * as fs from 'fs';
import * as path from 'path';

import * as cpp from 'child-process-promise';

import { ExtensionContext } from 'vscode';
import * as vscode from 'vscode';
import { ExecuteCommandParams, ExecuteCommandRequest, LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient';

import { Commands } from "./commands";
import { DottyDebugConfigurationProvider } from "./configurationProvider";

let extensionContext: ExtensionContext
let outputChannel: vscode.OutputChannel

export function activate(context: ExtensionContext) {
  extensionContext = context
  outputChannel = vscode.window.createOutputChannel('Dotty Language Client');

  context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider("dotty", new DottyDebugConfigurationProvider()));
  context.subscriptions.push(vscode.commands.registerCommand("DottyDebug.SpecifyProgramArgs", async () => {
    return specifyProgramArguments(context);
  }));

  const artifactFile = `${vscode.workspace.rootPath}/.dotty-ide-artifact`
  fs.readFile(artifactFile, (err, data) => {
    if (err) {
      outputChannel.append(`Unable to parse ${artifactFile}`)
      throw err
    }
    const artifact = data.toString().trim()

    if (process.env['DLS_DEV_MODE']) {
      const portFile = `${vscode.workspace.rootPath}/.dotty-ide-dev-port`
      fs.readFile(portFile, (err, port) => {
        if (err) {
          outputChannel.append(`Unable to parse ${portFile}`)
          throw err
        }

        run({
          module: context.asAbsolutePath('out/src/passthrough-server.js'),
          args: [ port.toString() ]
        })
      })
    } else {
      fetchAndRun(artifact)
    }
  })
}

function fetchAndRun(artifact: string) {
  const coursierPath = path.join(extensionContext.extensionPath, './out/coursier');

  vscode.window.withProgress({
    location: vscode.ProgressLocation.Window,
    title: 'Fetching the Dotty Language Server'
  }, (progress) => {

    const coursierPromise =
      cpp.spawn("java", [
        "-jar", coursierPath,
        "fetch",
        "-p",
        artifact
      ])
    const coursierProc = coursierPromise.childProcess

    let classPath = ""

    coursierProc.stdout.on('data', (data: Buffer) => {
      classPath += data.toString().trim()
    })
    coursierProc.stderr.on('data', (data: Buffer) => {
      let msg = data.toString()
      outputChannel.append(msg)
    })

    coursierProc.on('close', (code: number) => {
      if (code != 0) {
        let msg = "Fetching the language server failed."
        outputChannel.append(msg)
        throw new Error(msg)
      }

      run({
        command: "java",
        args: ["-classpath", classPath, "dotty.tools.languageserver.Main", "-stdio"]
      })
    })
    return coursierPromise
  })
}

function run(serverOptions: ServerOptions) {
  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { language: 'scala', scheme: 'file', pattern: '**/*.scala' },
      { language: 'scala', scheme: 'untitled', pattern: '**/*.scala' }
    ],
    synchronize: {
      configurationSection: 'dotty'
    }
  }

  outputChannel.dispose()

  const client = new LanguageClient('dotty', 'Dotty Language Server', serverOptions, clientOptions);
	client.onReady().then(() => {
	  vscode.commands.registerCommand(Commands.DOTTY_EXECUTE_WORKSPACE_COMMAND, (command, ...rest) => {
		  const params: ExecuteCommandParams = {
			  command,
			  arguments: rest
		  }
		  return client.sendRequest(ExecuteCommandRequest.type, params)
	  })
  })

  // Push the disposable to the context's subscriptions so that the
  // client can be deactivated on extension deactivation
  extensionContext.subscriptions.push(client.start());
  
}

function specifyProgramArguments(context: vscode.ExtensionContext): Thenable<string> {
    const dottyDebugProgramArgsKey = "DottyDebugProgramArgs";

    const options: vscode.InputBoxOptions = {
        ignoreFocusOut: true,
        placeHolder: "Enter program arguments or leave empty to pass no args",
    };

    const prevArgs = context.workspaceState.get(dottyDebugProgramArgsKey, "");
    if (prevArgs.length > 0) {
        options.value = prevArgs;
    }

    return vscode.window.showInputBox(options).then((text) => {
        // When user cancels the input box (by pressing Esc), the text value is undefined.
        if (text !== undefined) {
            context.workspaceState.update(dottyDebugProgramArgsKey, text);
        }

        return text || " ";
    });
}
