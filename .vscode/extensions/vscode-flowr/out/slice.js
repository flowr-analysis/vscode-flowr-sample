"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.registerSliceCommands = registerSliceCommands;
exports.displaySlice = displaySlice;
exports.makeSliceDecorationTypes = makeSliceDecorationTypes;
const vscode = require("vscode");
const extension_1 = require("./extension");
const settings_1 = require("./settings");
const selection_slicer_1 = require("./selection-slicer");
const position_slicer_1 = require("./position-slicer");
const doc_provider_1 = require("./doc-provider");
function registerSliceCommands(context) {
    context.subscriptions.push(vscode.commands.registerCommand('vscode-flowr.slice.cursor', async () => {
        return await (0, selection_slicer_1.getSelectionSlicer)().sliceSelectionOnce();
    }));
    context.subscriptions.push(vscode.commands.registerCommand('vscode-flowr.slice.clear', () => {
        clearSliceOutput();
    }));
    context.subscriptions.push(vscode.commands.registerCommand('vscode-flowr.slice.follow.cursor', async () => {
        await (0, selection_slicer_1.getSelectionSlicer)().toggleFollowSelection();
    }));
    context.subscriptions.push(vscode.commands.registerCommand('vscode-flowr.slice.show.in.editor', async () => {
        return await showReconstructionInEditor();
    }));
    context.subscriptions.push(vscode.commands.registerCommand('vscode-flowr.slice.position', async () => {
        await (0, position_slicer_1.addCurrentPositions)();
    }));
    vscode.workspace.onDidChangeConfiguration(e => {
        if (e.affectsConfiguration(`${settings_1.Settings.Category}`)) {
            const selSlicer = (0, selection_slicer_1.getSelectionSlicer)();
            selSlicer.clearSliceDecos();
            for (const [, positionSlicer] of position_slicer_1.positionSlicers) {
                void positionSlicer.updateOutput(true);
            }
        }
    });
}
async function showReconstructionInEditor() {
    const positionSlicer = (0, position_slicer_1.getActivePositionSlicer)();
    if (positionSlicer) {
        return await positionSlicer.showReconstruction();
    }
    return await (0, selection_slicer_1.showSelectionSliceInEditor)();
}
function clearSliceOutput() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        return;
    }
    const clearedPositionSlicer = (0, position_slicer_1.disposeActivePositionSlicer)();
    if (clearedPositionSlicer) {
        return;
    }
    const slicer = (0, selection_slicer_1.getSelectionSlicer)();
    slicer.clearSelectionSlice();
}
function displaySlice(editor, sliceElements, decos) {
    const sliceLines = new Set(sliceElements.map(s => s.location[0] - 1));
    switch ((0, extension_1.getConfig)().get(settings_1.Settings.StyleSliceDisplay)) {
        case 'tokens': {
            const ranges = [];
            for (const el of sliceElements) {
                const range = new vscode.Range(el.location[0] - 1, el.location[1] - 1, el.location[2] - 1, el.location[3]);
                ranges.push(range);
            }
            editor.setDecorations(decos.tokenSlice, ranges);
            break;
        }
        case 'text': {
            if (sliceLines.size === 0) {
                return; // do not grey out the entire document
            }
            const decorations = [];
            for (let i = 0; i < editor.document.lineCount; i++) {
                if (!sliceLines.has(i)) {
                    decorations.push({ range: new vscode.Range(i, 0, i, editor.document.lineAt(i).text.length) });
                }
            }
            editor.setDecorations(decos.lineSlice, decorations);
            break;
        }
        case 'diff': {
            const sliceContent = [];
            for (let i = 0; i < editor.document.lineCount; i++) {
                if (!sliceLines.has(i)) {
                    sliceContent.push(editor.document.lineAt(i).text);
                }
            }
            const uri = (0, doc_provider_1.makeUri)('slice-diff-view', 'Slice Diff View');
            (0, doc_provider_1.getReconstructionContentProvider)().updateContents(uri, sliceContent.join('\n'));
            void vscode.commands.executeCommand('vscode.diff', uri, editor.document.uri, 'Slice Diff View', { viewColumn: vscode.ViewColumn.Beside, preserveFocus: true });
            break;
        }
    }
}
function makeSliceDecorationTypes() {
    const config = (0, extension_1.getConfig)();
    const tokenColor = config.get('style.tokenBackgroundColor', 'green');
    const ret = {
        lineSlice: vscode.window.createTextEditorDecorationType({
            opacity: config.get(settings_1.Settings.StyleSliceOpacity)?.toString()
        }),
        tokenSlice: vscode.window.createTextEditorDecorationType({
            backgroundColor: `${tokenColor}`,
        }),
        slicedPos: vscode.window.createTextEditorDecorationType({
            before: {
                contentText: '\u2192',
                backgroundColor: `${tokenColor}`,
                border: `2px solid ${tokenColor}`,
            },
            border: `2px solid ${tokenColor}`,
        }),
        dispose() {
            this.lineSlice.dispose();
            this.tokenSlice.dispose();
            this.slicedPos.dispose();
        }
    };
    return ret;
}
//# sourceMappingURL=slice.js.map