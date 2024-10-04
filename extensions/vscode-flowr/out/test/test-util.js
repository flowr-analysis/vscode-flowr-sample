"use strict";
// also see https://github.com/microsoft/vscode-extension-samples/blob/main/lsp-sample/client/src/test/helper.ts for various test helper samples
Object.defineProperty(exports, "__esModule", { value: true });
exports.activateExtension = activateExtension;
exports.openTestFile = openTestFile;
const vscode = require("vscode");
const assert = require("assert");
const path = require("path");
async function activateExtension() {
    const ext = vscode.extensions.getExtension('code-Inspect.vscode-flowr');
    assert.notEqual(ext, undefined, 'extension not found');
    await assert.doesNotReject(async () => {
        await ext?.activate();
    }, 'extension activation failed');
    // force start a local shell and wait, since there seem to be some async issues with commands
    const session = await vscode.commands.executeCommand('vscode-flowr.session.internal');
    assert.equal(session.state, 'active');
}
async function openTestFile(name, selection) {
    const file = path.resolve(__dirname, '..', '..', 'test-workspace', name);
    const doc = await vscode.workspace.openTextDocument(file);
    const editor = await vscode.window.showTextDocument(doc);
    if (selection) {
        editor.selection = selection;
    }
    return editor;
}
//# sourceMappingURL=test-util.js.map