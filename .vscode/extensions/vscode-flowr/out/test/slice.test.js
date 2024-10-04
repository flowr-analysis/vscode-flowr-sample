"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const assert = require("assert");
const test_util_1 = require("./test-util");
suite('slice', () => {
    suiteSetup(async () => {
        await (0, test_util_1.activateExtension)();
    });
    test('slice cursor', async () => {
        await (0, test_util_1.openTestFile)('example.R', new vscode.Selection(7, 6, 7, 6));
        const slice = await vscode.commands.executeCommand('vscode-flowr.slice.cursor');
        assert.ok(slice);
        assert.equal(slice, `
product <- 1
n <- 10
for(i in 1:(n - 1)) product <- product * i
			`.trim());
    });
    test('reconstruct cursor', async () => {
        await (0, test_util_1.openTestFile)('example.R', new vscode.Selection(7, 6, 7, 6));
        const newEditor = await vscode.commands.executeCommand('vscode-flowr.slice.show.in.editor');
        assert.ok(newEditor);
        assert.equal(vscode.window.activeTextEditor, newEditor);
        assert.ok(newEditor.document.fileName.endsWith('Selection Slice'));
        assert.equal(newEditor.document.getText(), `
product <- 1
n <- 10
for(i in 1:(n - 1)) product <- product * i
			`.trim());
    });
});
//# sourceMappingURL=slice.test.js.map