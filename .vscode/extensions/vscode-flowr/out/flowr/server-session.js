"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.FlowrServerSession = void 0;
const net = require("net");
const vscode = require("vscode");
const ws = require("ws");
const extension_1 = require("../extension");
const settings_1 = require("../settings");
const dataflow_printer_1 = require("@eagleoutice/flowr-dev/core/print/dataflow-printer");
const cfg_1 = require("@eagleoutice/flowr-dev/util/cfg/cfg");
const ast_1 = require("@eagleoutice/flowr-dev/util/mermaid/ast");
const cfg_2 = require("@eagleoutice/flowr-dev/util/mermaid/cfg");
const utils_1 = require("./utils");
const visitor_1 = require("@eagleoutice/flowr-dev/r-bridge/lang-4.x/ast/model/processing/visitor");
const graph_1 = require("@eagleoutice/flowr-dev/dataflow/graph/graph");
const bimap_1 = require("@eagleoutice/flowr-dev/util/bimap");
class FlowrServerSession {
    state;
    flowrVersion;
    rVersion;
    outputChannel;
    connection;
    idCounter = 0;
    constructor(outputChannel) {
        this.outputChannel = outputChannel;
        this.state = 'inactive';
        (0, extension_1.updateStatusBar)();
    }
    async initialize() {
        this.state = 'connecting';
        (0, extension_1.updateStatusBar)();
        const configType = (0, extension_1.getConfig)().get(settings_1.Settings.ServerConnectionType, 'auto');
        this.connect(configType, configType);
        // the first response will be flowR's hello message
        return this.awaitResponse().then(r => {
            const info = JSON.parse(r);
            this.rVersion = info.versions.r;
            this.flowrVersion = info.versions.flowr;
            (0, extension_1.updateStatusBar)();
        });
    }
    destroy() {
        this.connection?.destroy();
    }
    connect(configType, typeToUse) {
        let host = (0, extension_1.getConfig)().get(settings_1.Settings.ServerHost, 'localhost');
        const port = (0, extension_1.getConfig)().get(settings_1.Settings.ServerPort, 1042);
        // we also set configType when overriding the type to use because that's the only one we want to try even in auto mode!
        if (host.startsWith('ws://')) {
            host = host.substring(5);
            configType = typeToUse = 'websocket';
        }
        else if (host.startsWith('wss://')) {
            host = host.substring(6);
            configType = typeToUse = 'websocket-secure';
        }
        const [base, suff] = splitHost(host);
        this.outputChannel.appendLine(`Connecting to flowR server using ${typeToUse} at ${base}:${port}/${suff}/`);
        // if the type is auto, we still start with a (secure!) websocket connection first
        this.connection = (0, extension_1.isWeb)() ? new BrowserWsConnection(typeToUse !== 'websocket') : typeToUse == 'tcp' ? new TcpConnection() : new WsConnection(typeToUse !== 'websocket');
        this.connection.connect(host, port, () => {
            this.state = 'connected';
            (0, extension_1.updateStatusBar)();
            this.outputChannel.appendLine('Connected to flowR server');
        });
        this.connection.on('error', e => {
            this.outputChannel.appendLine(`flowR server error: ${e.message}`);
            if (configType == 'auto' && this.connection instanceof WsConnection) {
                // retry with tcp if we're in auto mode and the ws secure and normal ws connections failed
                this.connect(configType, this.connection.secure ? 'websocket' : 'tcp');
            }
            else {
                this.state = 'inactive';
                (0, extension_1.updateStatusBar)();
                const useLocal = 'Use local shell instead';
                const openSettings = 'Open connection settings';
                void vscode.window.showErrorMessage(`The flowR server connection reported an error: ${e.message}`, openSettings, (0, extension_1.isWeb)() ? '' : useLocal)
                    .then(v => {
                    if (v === useLocal) {
                        void (0, extension_1.establishInternalSession)();
                    }
                    else if (v === openSettings) {
                        void vscode.commands.executeCommand('workbench.action.openSettings', 'vscode-flowr.server');
                    }
                });
            }
        });
        this.connection.on('close', () => {
            this.outputChannel.appendLine('flowR server connection closed');
            this.state = 'not connected';
            (0, extension_1.updateStatusBar)();
        });
        this.connection.on('data', str => this.handleResponse(String(str)));
    }
    currentMessageBuffer = '';
    handleResponse(message) {
        if (!message.endsWith('\n')) {
            this.currentMessageBuffer += message;
            return;
        }
        message = this.currentMessageBuffer + message;
        this.currentMessageBuffer = '';
        if ((0, extension_1.isVerbose)()) {
            this.outputChannel.appendLine('Received: ' + message);
        }
        this.onceOnLineReceived?.(message);
        this.onceOnLineReceived = undefined;
    }
    onceOnLineReceived;
    sendCommand(command) {
        if (this.connection) {
            if ((0, extension_1.isVerbose)()) {
                this.outputChannel.appendLine('Sending: ' + JSON.stringify(command));
            }
            this.connection.write(JSON.stringify(command) + '\n');
            return true;
        }
        return false;
    }
    async sendCommandWithResponse(command) {
        const response = this.awaitResponse();
        this.sendCommand(command);
        return JSON.parse(await response);
    }
    awaitResponse() {
        return new Promise(resolve => {
            this.onceOnLineReceived = resolve;
        });
    }
    async retrieveDataflowMermaid(document) {
        const response = await this.requestFileAnalysis(document);
        return (0, dataflow_printer_1.dataflowGraphToMermaid)({
            ...response.results.dataflow,
            graph: graph_1.DataflowGraph.fromJson(response.results.dataflow.graph)
        });
    }
    async retrieveAstMermaid(document) {
        const response = await this.requestFileAnalysis(document);
        return (0, ast_1.normalizedAstToMermaid)(response.results.normalize.ast);
    }
    async retrieveCfgMermaid(document) {
        const response = await this.requestFileAnalysis(document);
        const normalize = {
            ...response.results.normalize,
            idMap: new bimap_1.BiMap()
        };
        return (0, cfg_2.cfgToMermaid)((0, cfg_1.extractCFG)(normalize), normalize);
    }
    async retrieveSlice(positions, document) {
        const criteria = (0, utils_1.makeSlicingCriteria)(positions, document, (0, extension_1.isVerbose)());
        const response = await this.requestFileAnalysis(document);
        // now we want to collect all ids from response in a map again (id -> location)
        const idToLocation = new Map();
        (0, visitor_1.visitAst)(response.results.normalize.ast, n => {
            // backwards compat for server versions before 2.0.2, which used a "flavor" rather than a "named" boolean
            if (n.flavor === 'named') {
                n['name' + 'd'] = true;
            }
            if (n.location) {
                idToLocation.set(n.info.id, n.location);
            }
        });
        const sliceResponse = await this.sendCommandWithResponse({
            'type': 'request-slice',
            'id': String(this.idCounter++),
            'filetoken': '@tmp',
            'criterion': criteria
        });
        const sliceElements = (0, utils_1.makeSliceElements)(sliceResponse.results.slice.result, id => idToLocation.get(id));
        if ((0, extension_1.isVerbose)()) {
            this.outputChannel.appendLine('slice: ' + JSON.stringify([...sliceResponse.results.slice.result]));
        }
        return {
            code: sliceResponse.results.reconstruct.code,
            sliceElements
        };
    }
    async requestFileAnalysis(document) {
        return await this.sendCommandWithResponse({
            type: 'request-file-analysis',
            id: String(this.idCounter++),
            filename: document.fileName,
            format: 'json',
            filetoken: '@tmp',
            content: (0, utils_1.consolidateNewlines)(document.getText())
        });
    }
}
exports.FlowrServerSession = FlowrServerSession;
class TcpConnection {
    socket;
    connect(host, port, connectionListener) {
        this.socket = net.createConnection(port, host, connectionListener);
    }
    on(event, listener) {
        this.socket?.on(event, listener);
    }
    write(data) {
        this.socket?.write(data);
    }
    destroy() {
        this.socket?.destroy();
    }
}
/**
 * splits foo.com/bar into ['foo.com', 'bar']
 */
function splitHost(baseHost) {
    const split = baseHost.split('/');
    return [split[0], split.slice(1).join('/')];
}
class WsConnection {
    secure;
    socket;
    constructor(secure) {
        this.secure = secure;
    }
    connect(host, port, connectionListener) {
        const [base, suff] = splitHost(host);
        this.socket = new ws.WebSocket(`${this.secure ? 'wss' : 'ws'}://${base}:${port}/${suff}`);
        this.socket.on('open', connectionListener);
    }
    on(event, listener) {
        this.socket?.on(event == 'data' ? 'message' : event, listener);
    }
    write(data) {
        this.socket?.send(data);
    }
    destroy() {
        this.socket?.close();
    }
}
class BrowserWsConnection {
    secure;
    socket;
    constructor(secure) {
        this.secure = secure;
    }
    connect(host, port, connectionListener) {
        const [base, suff] = splitHost(host);
        this.socket = new WebSocket(`${this.secure ? 'wss' : 'ws'}://${base}:${port}/${suff}/`);
        this.socket.addEventListener('open', connectionListener);
    }
    on(event, listener) {
        this.socket?.addEventListener(event == 'data' ? 'message' : event, e => listener(e?.data ?? e));
    }
    write(data) {
        this.socket?.send(data);
    }
    destroy() {
        this.socket?.close();
    }
}
//# sourceMappingURL=server-session.js.map