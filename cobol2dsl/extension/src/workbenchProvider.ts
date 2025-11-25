/**
 * Dual-panel workbench provider for COBOL-to-DSL conversion
 * Manages webview-based side-by-side editor interface
 */

import * as vscode from 'vscode';
import { TranspilerBridge } from './transpilerBridge';
import { ConversionResult, WorkbenchState } from './types';

export class WorkbenchProvider implements vscode.WebviewViewProvider {
    public static readonly viewType = 'cobol-migration.workbench';

    private _view?: vscode.WebviewView;
    private state: WorkbenchState = {
        cobolContent: '',
        dslContent: '',
        accumulatedRules: []
    };

    constructor(
        private readonly _extensionUri: vscode.Uri,
        private readonly transpiler: TranspilerBridge
    ) {}

    public resolveWebviewView(
        webviewView: vscode.WebviewView,
        context: vscode.WebviewViewResolveContext,
        _token: vscode.CancellationToken
    ) {
        this._view = webviewView;

        webviewView.webview.options = {
            enableScripts: true,
            localResourceRoots: [this._extensionUri]
        };

        webviewView.webview.html = this._getHtmlForWebview(webviewView.webview);

        // Handle messages from webview
        webviewView.webview.onDidReceiveMessage(async (data) => {
            switch (data.type) {
                case 'convert':
                    await this.convertCOBOL(data.cobolCode, data.ruleName);
                    break;
                case 'saveState':
                    this.state = data.state;
                    break;
                case 'exportDSL':
                    await this.exportDSL(data.dslContent);
                    break;
            }
        });
    }

    /**
     * Convert COBOL code to DSL and update webview
     */
    private async convertCOBOL(cobolCode: string, ruleName?: string) {
        if (!this._view) {
            return;
        }

        try {
            // Show progress
            this._view.webview.postMessage({
                type: 'conversionStarted'
            });

            // Call transpiler
            const result: ConversionResult = await this.transpiler.convertToDSL(cobolCode, ruleName);

            // Append to accumulated rules
            this.state.accumulatedRules.push(result.dsl);
            this.state.dslContent += '\n\n' + result.dsl;

            // Send result back to webview
            this._view.webview.postMessage({
                type: 'conversionComplete',
                result: {
                    dsl: result.dsl,
                    metadata: result.metadata,
                    warnings: result.warnings,
                    errors: result.errors
                }
            });

        } catch (error) {
            // Send error to webview
            this._view.webview.postMessage({
                type: 'conversionError',
                error: error instanceof Error ? error.message : String(error)
            });
        }
    }

    /**
     * Export DSL content to a file
     */
    private async exportDSL(dslContent: string) {
        const uri = await vscode.window.showSaveDialog({
            filters: {
                'Rules DSL': ['rules', 'dsl'],
                'All Files': ['*']
            },
            defaultUri: vscode.Uri.file('converted_rules.rules')
        });

        if (uri) {
            await vscode.workspace.fs.writeFile(
                uri,
                Buffer.from(dslContent, 'utf8')
            );
            vscode.window.showInformationMessage(`DSL exported to ${uri.fsPath}`);
        }
    }

    /**
     * Generate HTML for the dual-panel webview
     */
    private _getHtmlForWebview(webview: vscode.Webview) {
        return `<!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>COBOL to DSL Migration Workbench</title>
            <style>
                body {
                    font-family: var(--vscode-font-family);
                    padding: 0;
                    margin: 0;
                    display: flex;
                    flex-direction: column;
                    height: 100vh;
                }
                .header {
                    padding: 10px;
                    background-color: var(--vscode-editor-background);
                    border-bottom: 1px solid var(--vscode-panel-border);
                }
                .container {
                    display: flex;
                    flex: 1;
                    overflow: hidden;
                }
                .panel {
                    flex: 1;
                    display: flex;
                    flex-direction: column;
                    padding: 10px;
                }
                .panel-title {
                    font-weight: bold;
                    margin-bottom: 10px;
                    color: var(--vscode-foreground);
                }
                textarea {
                    flex: 1;
                    font-family: 'Courier New', monospace;
                    font-size: 12px;
                    background-color: var(--vscode-editor-background);
                    color: var(--vscode-editor-foreground);
                    border: 1px solid var(--vscode-input-border);
                    padding: 8px;
                    resize: none;
                }
                .divider {
                    width: 2px;
                    background-color: var(--vscode-panel-border);
                }
                button {
                    background-color: var(--vscode-button-background);
                    color: var(--vscode-button-foreground);
                    border: none;
                    padding: 6px 14px;
                    cursor: pointer;
                    margin-right: 8px;
                }
                button:hover {
                    background-color: var(--vscode-button-hoverBackground);
                }
                .metadata {
                    margin-top: 10px;
                    padding: 8px;
                    background-color: var(--vscode-textBlockQuote-background);
                    border-left: 3px solid var(--vscode-textLink-foreground);
                    font-size: 11px;
                    max-height: 150px;
                    overflow-y: auto;
                }
                .error {
                    color: var(--vscode-errorForeground);
                    padding: 8px;
                    margin-top: 10px;
                }
                .warning {
                    color: var(--vscode-editorWarning-foreground);
                }
            </style>
        </head>
        <body>
            <div class="header">
                <input type="text" id="ruleName" placeholder="Rule Name (optional)"
                    style="width: 300px; padding: 4px; margin-right: 10px;" />
                <button onclick="convertSelection()">Convert Selected COBOL</button>
                <button onclick="convertAll()">Convert All</button>
                <button onclick="exportDSL()">Export DSL</button>
                <button onclick="clearAll()">Clear All</button>
            </div>
            <div class="container">
                <div class="panel">
                    <div class="panel-title">📄 COBOL Source</div>
                    <textarea id="cobolEditor" placeholder="Paste or type COBOL code here..."></textarea>
                </div>
                <div class="divider"></div>
                <div class="panel">
                    <div class="panel-title">✨ Rules DSL Output</div>
                    <textarea id="dslEditor" readonly placeholder="Converted DSL will appear here..."></textarea>
                    <div id="metadata" class="metadata" style="display: none;"></div>
                    <div id="error" class="error" style="display: none;"></div>
                </div>
            </div>
            <script>
                const vscode = acquireVsCodeApi();

                function convertSelection() {
                    const cobolEditor = document.getElementById('cobolEditor');
                    const selection = cobolEditor.value.substring(
                        cobolEditor.selectionStart,
                        cobolEditor.selectionEnd
                    );
                    const cobolCode = selection || cobolEditor.value;
                    const ruleName = document.getElementById('ruleName').value;

                    if (!cobolCode.trim()) {
                        showError('No COBOL code to convert');
                        return;
                    }

                    vscode.postMessage({
                        type: 'convert',
                        cobolCode: cobolCode,
                        ruleName: ruleName
                    });
                }

                function convertAll() {
                    const cobolEditor = document.getElementById('cobolEditor');
                    const ruleName = document.getElementById('ruleName').value;
                    vscode.postMessage({
                        type: 'convert',
                        cobolCode: cobolEditor.value,
                        ruleName: ruleName
                    });
                }

                function exportDSL() {
                    const dslEditor = document.getElementById('dslEditor');
                    vscode.postMessage({
                        type: 'exportDSL',
                        dslContent: dslEditor.value
                    });
                }

                function clearAll() {
                    document.getElementById('cobolEditor').value = '';
                    document.getElementById('dslEditor').value = '';
                    document.getElementById('metadata').style.display = 'none';
                    document.getElementById('error').style.display = 'none';
                }

                function showError(message) {
                    const errorDiv = document.getElementById('error');
                    errorDiv.textContent = 'Error: ' + message;
                    errorDiv.style.display = 'block';
                }

                // Handle messages from extension
                window.addEventListener('message', event => {
                    const message = event.data;
                    switch (message.type) {
                        case 'conversionStarted':
                            document.getElementById('error').style.display = 'none';
                            document.getElementById('metadata').textContent = 'Converting...';
                            document.getElementById('metadata').style.display = 'block';
                            break;

                        case 'conversionComplete':
                            const dslEditor = document.getElementById('dslEditor');
                            dslEditor.value += (dslEditor.value ? '\\n\\n' : '') + message.result.dsl;

                            // Show metadata
                            const metadataDiv = document.getElementById('metadata');
                            let metadataHTML = '<strong>Conversion Metadata:</strong><br>';
                            metadataHTML += 'Mappings: ' + message.result.metadata.mappings.length + '<br>';
                            message.result.metadata.mappings.forEach(m => {
                                metadataHTML += '  &bull; ' + m.cobol + ' -> ' + m.target + ' (' + m.type + ', conf: ' + m.confidence + ')<br>';
                            });
                            if (message.result.warnings) {
                                metadataHTML += '<div class="warning">Warnings: ' + message.result.warnings.join(', ') + '</div>';
                            }
                            metadataDiv.innerHTML = metadataHTML;
                            metadataDiv.style.display = 'block';
                            break;

                        case 'conversionError':
                            showError(message.error);
                            break;
                    }
                });
            </script>
        </body>
        </html>`;
    }
}
