"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.BEST_R_MAJOR = exports.MINIMUM_R_MAJOR = void 0;
exports.activate = activate;
exports.getConfig = getConfig;
exports.isVerbose = isVerbose;
exports.establishInternalSession = establishInternalSession;
exports.getFlowrSession = getFlowrSession;
exports.establishServerSession = establishServerSession;
exports.destroySession = destroySession;
exports.updateStatusBar = updateStatusBar;
exports.isWeb = isWeb;
const vscode = require("vscode");
const internal_session_1 = require("./flowr/internal-session");
const server_session_1 = require("./flowr/server-session");
const settings_1 = require("./settings");
const slice_1 = require("./slice");
const diagram_1 = require("./diagram");
const selection_slicer_1 = require("./selection-slicer");
const position_slicer_1 = require("./position-slicer");
const version_1 = require("@eagleoutice/flowr-dev/util/version");
exports.MINIMUM_R_MAJOR = 3;
exports.BEST_R_MAJOR = 4;
let outputChannel;
let statusBarItem;
let flowrSession;
async function activate(context) {
    console.log('Loading vscode-flowr');
    outputChannel = vscode.window.createOutputChannel('flowR');
    (0, diagram_1.registerDiagramCommands)(context, outputChannel);
    (0, slice_1.registerSliceCommands)(context);
    context.subscriptions.push(vscode.commands.registerCommand('vscode-flowr.session.internal', async () => {
        await establishInternalSession();
        return flowrSession;
    }));
    context.subscriptions.push(vscode.commands.registerCommand('vscode-flowr.session.connect', async () => {
        await establishServerSession();
        return flowrSession;
    }));
    context.subscriptions.push(vscode.commands.registerCommand('vscode-flowr.session.disconnect', () => {
        if (flowrSession instanceof server_session_1.FlowrServerSession) {
            destroySession();
        }
    }));
    context.subscriptions.push(vscode.commands.registerCommand('vscode-flowr.report', () => {
        void vscode.env.openExternal(vscode.Uri.parse('https://github.com/flowr-analysis/flowr/issues/new/choose'));
    }));
    statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
    context.subscriptions.push(statusBarItem);
    updateStatusBar();
    context.subscriptions.push(new vscode.Disposable(() => destroySession()));
    process.on('SIGINT', () => destroySession());
    if (getConfig().get(settings_1.Settings.ServerAutoConnect)) {
        await establishServerSession();
    }
}
function getConfig() {
    return vscode.workspace.getConfiguration(settings_1.Settings.Category);
}
function isVerbose() {
    return getConfig().get(settings_1.Settings.VerboseLog, false);
}
async function establishInternalSession() {
    destroySession();
    flowrSession = new internal_session_1.FlowrInternalSession(outputChannel);
    await flowrSession.initialize();
    return flowrSession;
}
async function getFlowrSession() {
    if (flowrSession) {
        return flowrSession;
    }
    // on the web, we always want to connect to a server since we don't support local sessions
    return await (isWeb() ? establishServerSession() : establishInternalSession());
}
async function establishServerSession() {
    destroySession();
    flowrSession = new server_session_1.FlowrServerSession(outputChannel);
    await flowrSession.initialize();
    return flowrSession;
}
function destroySession() {
    flowrSession?.destroy();
    flowrSession = undefined;
}
function updateStatusBar() {
    const text = [];
    const tooltip = [];
    if (flowrSession instanceof server_session_1.FlowrServerSession) {
        text.push(`$(cloud) flowR ${flowrSession.state}`);
        if (flowrSession.state === 'connected') {
            tooltip.push(`R version ${flowrSession.rVersion}  \nflowR version ${flowrSession.flowrVersion}`);
        }
    }
    else if (flowrSession instanceof internal_session_1.FlowrInternalSession) {
        text.push(`$(console) flowR ${flowrSession.state}`);
        if (flowrSession.state === 'active') {
            tooltip.push(`R version ${flowrSession.rVersion}  \nflowR version ${(0, version_1.flowrVersion)().toString()}`);
        }
    }
    const slicingTypes = [];
    const slicingFiles = [];
    if (selection_slicer_1.selectionSlicer?.changeListeners.length) {
        slicingTypes.push('cursor');
    }
    if (position_slicer_1.positionSlicers.size) {
        slicingTypes.push(`${[...position_slicer_1.positionSlicers].reduce((i, [, s]) => i + s.offsets.length, 0)} positions`);
        for (const [doc, slicer] of position_slicer_1.positionSlicers) {
            slicingFiles.push(`${vscode.workspace.asRelativePath(doc.fileName)} (${slicer.offsets.length} positions)`);
        }
    }
    if (slicingTypes.length) {
        text.push(`$(lightbulb) Slicing ${slicingTypes.join(', ')}`);
        if (slicingFiles.length) {
            tooltip.push(`Slicing in\n${slicingFiles.map(f => `- ${f}`).join('\n')}`);
        }
    }
    if (text.length) {
        statusBarItem.show();
        statusBarItem.text = text.join(' ');
        statusBarItem.tooltip = tooltip.length ? tooltip.reduce((m, s) => m.appendMarkdown('\n\n').appendMarkdown(s), new vscode.MarkdownString()) : undefined;
    }
    else {
        statusBarItem.hide();
    }
}
function isWeb() {
    // apparently there is no official way to test this from the vscode api other
    // than in the command availability context stuff, which is not what we want
    // this is dirty but it should work since the WebSocket is unavailable in node
    return typeof WebSocket !== 'undefined';
}
//# sourceMappingURL=extension.js.map