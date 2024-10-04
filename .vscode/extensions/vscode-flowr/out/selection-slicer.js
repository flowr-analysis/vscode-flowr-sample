"use strict";
// Contains the class and some functions that are used to
// slice at the current cursor position
// (either per command or updating as the cursor moves)
Object.defineProperty(exports, "__esModule", { value: true });
exports.selectionSlicer = void 0;
exports.getSelectionSlicer = getSelectionSlicer;
exports.showSelectionSliceInEditor = showSelectionSliceInEditor;
const vscode = require("vscode");
const extension_1 = require("./extension");
const doc_provider_1 = require("./doc-provider");
const slice_1 = require("./slice");
const position_slicer_1 = require("./position-slicer");
const settings_1 = require("./settings");
const selectionSlicerAuthority = 'selection-slicer';
const selectionSlicerPath = 'Selection Slice';
function getSelectionSlicer() {
    exports.selectionSlicer ||= new SelectionSlicer();
    return exports.selectionSlicer;
}
// Show the selection slice in an editor
// If nothing is sliced, slice at the current cursor position
async function showSelectionSliceInEditor() {
    const slicer = getSelectionSlicer();
    if (!slicer.hasDoc) {
        await slicer.sliceSelectionOnce();
    }
    const uri = slicer.makeUri();
    return await (0, doc_provider_1.showUri)(uri);
}
class SelectionSlicer {
    changeListeners = [];
    hasDoc = false;
    decos;
    decoratedEditors = [];
    // Turn on/off following of the cursor
    async startFollowSelection() {
        await this.update();
        this.changeListeners.push(vscode.window.onDidChangeTextEditorSelection(e => {
            if (this.decoratedEditors.includes(e.textEditor)) {
                void this.update();
            }
        }), vscode.window.onDidChangeActiveTextEditor(() => void this.update()));
        (0, extension_1.updateStatusBar)();
    }
    async toggleFollowSelection() {
        if (this.changeListeners.length) {
            this.stopFollowSelection();
        }
        else {
            await this.startFollowSelection();
        }
    }
    stopFollowSelection() {
        while (this.changeListeners.length) {
            this.changeListeners.pop()?.dispose();
        }
        (0, extension_1.updateStatusBar)();
    }
    // Slice once at the current cursor position
    async sliceSelectionOnce() {
        return await this.update();
    }
    // Stop following the cursor and clear the selection slice output
    clearSelectionSlice() {
        this.stopFollowSelection();
        const provider = (0, doc_provider_1.getReconstructionContentProvider)();
        const uri = this.makeUri();
        provider.updateContents(uri, '');
        this.clearSliceDecos();
        this.hasDoc = false;
    }
    makeUri() {
        return (0, doc_provider_1.makeUri)(selectionSlicerAuthority, selectionSlicerPath);
    }
    // Clear all slice decos or only the ones affecting a specific editor/document
    clearSliceDecos(editor, doc) {
        if (!this.decos) {
            return;
        }
        if (editor) {
            editor.setDecorations(this.decos.lineSlice, []);
            editor.setDecorations(this.decos.tokenSlice, []);
            if (!doc) {
                return;
            }
        }
        if (doc) {
            for (const editor of vscode.window.visibleTextEditors) {
                if (editor.document === doc) {
                    this.clearSliceDecos(editor);
                }
            }
            return;
        }
        this.decos?.dispose();
        this.decos = undefined;
    }
    async update() {
        const ret = await getSelectionSlice();
        if (ret === undefined) {
            return '';
        }
        const provider = (0, doc_provider_1.getReconstructionContentProvider)();
        const uri = this.makeUri();
        provider.updateContents(uri, ret.code);
        this.hasDoc = true;
        const clearOtherDecos = (0, extension_1.getConfig)().get(settings_1.Settings.StyleOnlyHighlightActiveSelection, false);
        for (const editor of this.decoratedEditors) {
            if (editor === ret.editor) {
                continue;
            }
            if (clearOtherDecos || position_slicer_1.positionSlicers.has(editor.document)) {
                this.clearSliceDecos(editor);
            }
        }
        this.decos ||= (0, slice_1.makeSliceDecorationTypes)();
        (0, slice_1.displaySlice)(ret.editor, ret.sliceElements, this.decos);
        this.decoratedEditors.push(ret.editor);
        return ret.code;
    }
}
async function getSelectionSlice() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        return undefined;
    }
    if (editor.document.uri.scheme === doc_provider_1.flowrScheme) {
        return undefined;
    }
    if (editor.document.languageId.toLowerCase() !== 'r') {
        return undefined;
    }
    if (position_slicer_1.positionSlicers.has(editor.document)) {
        return undefined;
    }
    const positions = editor.selections.map(sel => sel.active);
    if (!positions.length) {
        // (should not happen)
        return undefined;
    }
    const flowrSession = await (0, extension_1.getFlowrSession)();
    const ret = await flowrSession.retrieveSlice(positions, editor.document, false);
    if (!ret.sliceElements.length) {
        return {
            code: '# No slice',
            sliceElements: [],
            editor: editor
        };
    }
    return {
        ...ret,
        editor
    };
}
//# sourceMappingURL=selection-slicer.js.map