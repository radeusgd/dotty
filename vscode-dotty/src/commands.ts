// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT license.

import * as vscode from "vscode";

export const VSCODE_STARTDEBUG = "vscode.startDebug";

export const VSCODE_ADD_DEBUGCONFIGURATION = "debug.addConfiguration";

export const DOTTY_START_DEBUGSESSION = "vscode.dotty.startDebugSession";

export const DOTTY_RESOLVE_CLASSPATH = "vscode.dotty.resolveClasspath";

export const DOTTY_RESOLVE_MAINCLASS = "vscode.dotty.resolveMainClass";

export const DOTTY_BUILD_WORKSPACE = "dotty.workspace.compile";

export const DOTTY_EXECUTE_WORKSPACE_COMMAND = "dotty.execute.workspaceCommand";

export const DOTTY_FETCH_USAGE_DATA = "vscode.dotty.fetchUsageData";

export const DOTTY_UPDATE_DEBUG_SETTINGS = "vscode.dotty.updateDebugSettings";

export function executeDottyLanguageServerCommand(...rest: string[]) {
    // TODO: need to handle error and trace telemetry
    return vscode.commands.executeCommand(DOTTY_EXECUTE_WORKSPACE_COMMAND, ...rest);
}
