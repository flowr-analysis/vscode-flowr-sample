"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const assert = require("assert");
const test_util_1 = require("./test-util");
suite('diagram', () => {
    suiteSetup(async () => {
        await (0, test_util_1.activateExtension)();
    });
    test('dataflow', async () => {
        await (0, test_util_1.openTestFile)('simple-example.R');
        const result = await vscode.commands.executeCommand('vscode-flowr.dataflow');
        assert.ok(result);
        assert.equal(result.webview.title, 'Dataflow Graph');
        assert.ok(result.mermaid.startsWith('flowchart TD'));
    });
});
//# sourceMappingURL=diagram.test.js.map