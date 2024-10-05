"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.FlowrInternalSession = void 0;
const vscode = require("vscode");
const extension_1 = require("../extension");
const settings_1 = require("../settings");
const dataflow_printer_1 = require("@eagleoutice/flowr-dev/core/print/dataflow-printer");
const cfg_1 = require("@eagleoutice/flowr-dev/util/cfg/cfg");
const utils_1 = require("./utils");
const shell_1 = require("@eagleoutice/flowr-dev/r-bridge/shell");
const pipeline_executor_1 = require("@eagleoutice/flowr-dev/core/pipeline-executor");
const default_pipelines_1 = require("@eagleoutice/flowr-dev/core/steps/pipeline/default-pipelines");
const retriever_1 = require("@eagleoutice/flowr-dev/r-bridge/retriever");
const ast_1 = require("@eagleoutice/flowr-dev/util/mermaid/ast");
const cfg_2 = require("@eagleoutice/flowr-dev/util/mermaid/cfg");
class FlowrInternalSession {
    state;
    rVersion;
    outputChannel;
    shell;
    constructor(outputChannel) {
        this.outputChannel = outputChannel;
        this.state = 'inactive';
        (0, extension_1.updateStatusBar)();
    }
    async initialize() {
        this.state = 'loading';
        (0, extension_1.updateStatusBar)();
        this.outputChannel.appendLine('Starting flowR shell');
        let options = {
            revive: 2 /* RShellReviveOptions.Always */,
            sessionName: 'flowr - vscode'
        };
        const executable = (0, extension_1.getConfig)().get(settings_1.Settings.Rexecutable)?.trim();
        if (executable !== undefined && executable.length > 0) {
            options = { ...options, pathToRExecutable: executable };
        }
        this.outputChannel.appendLine(`Using options ${JSON.stringify(options)}`);
        this.shell = new shell_1.RShell(options);
        this.shell.tryToInjectHomeLibPath();
        // wait at most 1 second for the version, since the R shell doesn't let us know if the path
        // we provided doesn't actually lead anywhere, or doesn't contain an R executable, etc.
        let handle;
        const timeout = new Promise(resolve => handle = setTimeout(() => resolve(null), 5000));
        await Promise.race([this.shell.usedRVersion(), timeout]).then(version => {
            clearTimeout(handle);
            if (!version) {
                const seeDoc = 'See documentation';
                void vscode.window.showErrorMessage('The R version could not be determined. R needs to be installed and part of your PATH environment variable.', seeDoc)
                    .then(s => {
                    if (s === seeDoc) {
                        void vscode.env.openExternal(vscode.Uri.parse('https://github.com/flowr-analysis/vscode-flowr/blob/main/README.md#using'));
                    }
                });
                this.state = 'failure';
                (0, extension_1.updateStatusBar)();
            }
            else {
                this.outputChannel.appendLine(`Using R version ${version.toString()}`);
                if (version.major < extension_1.MINIMUM_R_MAJOR) {
                    void vscode.window.showErrorMessage(`You are using R version ${version.toString()}, but ${extension_1.MINIMUM_R_MAJOR}.0.0 or higher is required.`);
                }
                else if (version.major < extension_1.BEST_R_MAJOR) {
                    void vscode.window.showWarningMessage(`You are using R version ${version.toString()}, which flowR has not been tested for. Version ${extension_1.BEST_R_MAJOR}.0.0 or higher is recommended.`);
                }
                this.state = 'active';
                this.rVersion = version.toString();
                (0, extension_1.updateStatusBar)();
            }
        });
    }
    destroy() {
        this.shell?.close();
    }
    async retrieveSlice(positions, document, showErrorMessage = true) {
        if (!this.shell) {
            return {
                code: '',
                sliceElements: []
            };
        }
        try {
            return await this.extractSlice(this.shell, document, positions);
        }
        catch (e) {
            this.outputChannel.appendLine('Error: ' + e?.message);
            e.stack?.split('\n').forEach(l => this.outputChannel.appendLine(l));
            if (showErrorMessage) {
                void vscode.window.showErrorMessage(`There was an error while extracting a slice: ${e?.message}. See the flowR output for more information.`);
            }
            return {
                code: '',
                sliceElements: []
            };
        }
    }
    async retrieveDataflowMermaid(document) {
        if (!this.shell) {
            return '';
        }
        const result = await new pipeline_executor_1.PipelineExecutor(default_pipelines_1.DEFAULT_DATAFLOW_PIPELINE, {
            shell: this.shell,
            request: (0, retriever_1.requestFromInput)((0, utils_1.consolidateNewlines)(document.getText()))
        }).allRemainingSteps();
        return (0, dataflow_printer_1.dataflowGraphToMermaid)(result.dataflow);
    }
    async retrieveAstMermaid(document) {
        if (!this.shell) {
            return '';
        }
        const result = await new pipeline_executor_1.PipelineExecutor(default_pipelines_1.DEFAULT_NORMALIZE_PIPELINE, {
            shell: this.shell,
            request: (0, retriever_1.requestFromInput)((0, utils_1.consolidateNewlines)(document.getText()))
        }).allRemainingSteps();
        return (0, ast_1.normalizedAstToMermaid)(result.normalize.ast);
    }
    async retrieveCfgMermaid(document) {
        if (!this.shell) {
            return '';
        }
        const result = await new pipeline_executor_1.PipelineExecutor(default_pipelines_1.DEFAULT_NORMALIZE_PIPELINE, {
            shell: this.shell,
            request: (0, retriever_1.requestFromInput)((0, utils_1.consolidateNewlines)(document.getText()))
        }).allRemainingSteps();
        return (0, cfg_2.cfgToMermaid)((0, cfg_1.extractCFG)(result.normalize), result.normalize);
    }
    async extractSlice(shell, document, positions) {
        const content = (0, utils_1.consolidateNewlines)(document.getText());
        const criteria = (0, utils_1.makeSlicingCriteria)(positions, document, (0, extension_1.isVerbose)());
        const slicer = new pipeline_executor_1.PipelineExecutor(default_pipelines_1.DEFAULT_SLICING_PIPELINE, {
            criterion: criteria,
            shell,
            request: (0, retriever_1.requestFromInput)(content)
        });
        const result = await slicer.allRemainingSteps();
        const sliceElements = (0, utils_1.makeSliceElements)(result.slice.result, id => result.normalize.idMap.get(id)?.location);
        if ((0, extension_1.isVerbose)()) {
            this.outputChannel.appendLine('slice: ' + JSON.stringify([...result.slice.result]));
        }
        return {
            code: result.reconstruct.code,
            sliceElements
        };
    }
}
exports.FlowrInternalSession = FlowrInternalSession;
//# sourceMappingURL=internal-session.js.map