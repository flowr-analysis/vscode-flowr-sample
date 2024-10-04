"use strict";
// The class in this file is used to provide content for the reconstruction editor
//
// The content of files is updated by us using the .updateContents() method.
//
// The content of a file is requested by vscode using the .provideTextDocumentContent() method,
// when the corresponding URI is opened.
//
// To show a file, use the showUri function, defined below.
Object.defineProperty(exports, "__esModule", { value: true });
exports.ReconstructionContentProvider = exports.flowrScheme = void 0;
exports.makeUri = makeUri;
exports.showUri = showUri;
exports.getReconstructionContentProvider = getReconstructionContentProvider;
const vscode = require("vscode");
exports.flowrScheme = 'flowr';
class ReconstructionContentProvider {
    listeners = [];
    contents = new Map();
    onDidChange(listener) {
        this.listeners.push(listener);
        const dispo = new vscode.Disposable(() => {
            this.listeners = this.listeners.filter(l => l !== listener);
        });
        return dispo;
    }
    notifyListeners(uri) {
        for (const listener of this.listeners) {
            listener(uri);
        }
    }
    updateContents(uri, content) {
        if (content !== undefined) {
            this.contents.set(uri.toString(), content);
        }
        else {
            this.contents.delete(uri.toString());
        }
        this.notifyListeners(uri);
    }
    provideTextDocumentContent(uri) {
        return this.contents.get(uri.toString());
    }
}
exports.ReconstructionContentProvider = ReconstructionContentProvider;
function makeUri(authority, path) {
    if (authority && path && !path.startsWith('/')) {
        path = '/' + path;
    }
    const uri = vscode.Uri.from({
        scheme: exports.flowrScheme,
        authority: authority,
        path: path
    });
    return uri;
}
async function showUri(uri, language = 'r', viewColumn = vscode.ViewColumn.Beside) {
    for (const editor of vscode.window.visibleTextEditors) {
        if (editor.document.uri.toString() === uri.toString()) {
            return editor;
        }
    }
    const doc = await vscode.workspace.openTextDocument(uri);
    await vscode.languages.setTextDocumentLanguage(doc, language);
    return await vscode.window.showTextDocument(doc, {
        viewColumn: viewColumn
    });
}
let reconstructionContentProvider;
function getReconstructionContentProvider() {
    if (!reconstructionContentProvider) {
        reconstructionContentProvider = new ReconstructionContentProvider();
        vscode.workspace.registerTextDocumentContentProvider(exports.flowrScheme, reconstructionContentProvider);
    }
    return reconstructionContentProvider;
}
//# sourceMappingURL=doc-provider.js.map