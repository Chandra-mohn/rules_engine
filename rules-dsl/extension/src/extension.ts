import * as vscode from 'vscode';
import { CLIClient } from './cli-client';
import { RulesCompletionProvider } from './providers/completion';
import { RulesHoverProvider } from './providers/hover';
import { RulesDiagnosticProvider } from './providers/diagnostics';
import { RulesFormattingProvider } from './providers/formatting';
import { RulesSignatureHelpProvider } from './providers/signature-help';
import { SchemaTreeDataProvider } from './providers/schema-tree';
import { ContextTreeDataProvider } from './providers/context-tree';
import { WorkspaceConfig } from './workspace-config';

function getTimestamp(): string {
    const now = new Date();
    const hours = String(now.getHours()).padStart(2, '0');
    const minutes = String(now.getMinutes()).padStart(2, '0');
    const seconds = String(now.getSeconds()).padStart(2, '0');
    const period = now.getHours() >= 12 ? 'PM' : 'AM';
    const displayHours = now.getHours() % 12 || 12;
    return `[${String(displayHours).padStart(2, '0')}:${minutes}:${seconds} ${period}]`;
}

export function activate(context: vscode.ExtensionContext) {
    console.log('Rules DSL extension activated');

    // Load workspace configuration
    const workspaceConfig = new WorkspaceConfig();
    workspaceConfig.load();

    // Initialize CLI client for code generation and validation
    // Pass extension context for reliable path resolution
    const cliClient = new CLIClient(context);

    // Create Output Channel for build logs
    const buildOutputChannel = vscode.window.createOutputChannel('Rules DSL Build');
    context.subscriptions.push(buildOutputChannel);

    // Create Status Bar Item for build status
    const buildStatusBar = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
    buildStatusBar.command = 'rules.showBuildOutput';
    buildStatusBar.text = '$(check) Build';
    buildStatusBar.tooltip = 'Rules DSL Build Status - Click to show output';
    buildStatusBar.show();
    context.subscriptions.push(buildStatusBar);

    // Register language features
    const rulesSelector: vscode.DocumentSelector = { language: 'rules', scheme: 'file' };

    // Completion provider (autocomplete)
    // Note: Trigger characters '.' and ' ' removed to enable completion on all typing
    // The provider itself filters what to show based on context
    context.subscriptions.push(
        vscode.languages.registerCompletionItemProvider(
            rulesSelector,
            new RulesCompletionProvider(workspaceConfig)
        )
    );

    // Hover provider (documentation)
    context.subscriptions.push(
        vscode.languages.registerHoverProvider(
            rulesSelector,
            new RulesHoverProvider(workspaceConfig)
        )
    );

    // Formatting provider (auto-indent)
    context.subscriptions.push(
        vscode.languages.registerDocumentFormattingEditProvider(
            rulesSelector,
            new RulesFormattingProvider()
        )
    );

    // Signature help provider (function parameters)
    context.subscriptions.push(
        vscode.languages.registerSignatureHelpProvider(
            rulesSelector,
            new RulesSignatureHelpProvider(),
            '(', ','  // Trigger on opening paren and comma
        )
    );

    // Diagnostic provider (validation)
    const diagnosticProvider = new RulesDiagnosticProvider();
    context.subscriptions.push(diagnosticProvider);

    // Update diagnostics on document open/change/save
    context.subscriptions.push(
        vscode.workspace.onDidOpenTextDocument(doc => {
            diagnosticProvider.updateDiagnostics(doc);
        })
    );

    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument(event => {
            diagnosticProvider.updateDiagnostics(event.document);
        })
    );

    context.subscriptions.push(
        vscode.workspace.onDidSaveTextDocument(doc => {
            diagnosticProvider.updateDiagnostics(doc);
        })
    );

    context.subscriptions.push(
        vscode.workspace.onDidCloseTextDocument(doc => {
            diagnosticProvider.clear();
        })
    );

    // Validate all open documents on activation
    vscode.workspace.textDocuments.forEach(doc => {
        diagnosticProvider.updateDiagnostics(doc);
    });

    // Tree Views
    const schemaTreeProvider = new SchemaTreeDataProvider(workspaceConfig);
    const contextTreeProvider = new ContextTreeDataProvider(workspaceConfig);

    context.subscriptions.push(
        vscode.window.registerTreeDataProvider('rulesSchemas', schemaTreeProvider)
    );

    context.subscriptions.push(
        vscode.window.registerTreeDataProvider('rulesContexts', contextTreeProvider)
    );

    // Tree View Commands
    context.subscriptions.push(
        vscode.commands.registerCommand('rules.refreshSchemas', () => {
            workspaceConfig.load();
            schemaTreeProvider.refresh();
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('rules.refreshContexts', () => {
            workspaceConfig.load();
            contextTreeProvider.refresh();
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('rules.insertAttribute', (entityName: string, attributeName: string) => {
            const editor = vscode.window.activeTextEditor;
            if (!editor) {
                return;
            }

            const position = editor.selection.active;
            editor.edit(editBuilder => {
                editBuilder.insert(position, `${entityName}.${attributeName}`);
            });
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('rules.attachContextFromTree', async (contextKey: string) => {
            const editor = vscode.window.activeTextEditor;
            if (!editor || editor.document.languageId !== 'rules') {
                vscode.window.showWarningMessage('No active .rules file');
                return;
            }

            // Get context to retrieve the name
            const contextData = workspaceConfig.getContext(contextKey);
            if (!contextData) {
                vscode.window.showErrorMessage(`Context not found: ${contextKey}`);
                return;
            }

            // Insert frontmatter with context
            const edit = new vscode.WorkspaceEdit();
            const firstLine = editor.document.lineAt(0);
            const frontmatter = `---\ncontext: ${contextKey}\n---\n\n`;

            edit.insert(editor.document.uri, new vscode.Position(0, 0), frontmatter);
            await vscode.workspace.applyEdit(edit);
            vscode.window.showInformationMessage(`✅ Attached context: ${contextData.name}`);
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('rules.insertContextField', (entityName: string, fieldName: string) => {
            const editor = vscode.window.activeTextEditor;
            if (!editor) {
                return;
            }

            const position = editor.selection.active;
            editor.edit(editBuilder => {
                editBuilder.insert(position, `${entityName}.${fieldName}`);
            });
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('rules.findAttribute', async () => {
            const schemas = workspaceConfig.getAllSchemas();
            const items: vscode.QuickPickItem[] = [];

            schemas.forEach((schema, entityName) => {
                schema.attributes.forEach(attr => {
                    items.push({
                        label: `${entityName}.${attr.name}`,
                        description: attr.type,
                        detail: attr.description
                    });
                });
            });

            const selected = await vscode.window.showQuickPick(items, {
                placeHolder: 'Search for an attribute across all entities',
                matchOnDescription: true,
                matchOnDetail: true
            });

            if (selected) {
                const editor = vscode.window.activeTextEditor;
                if (editor) {
                    const position = editor.selection.active;
                    editor.edit(editBuilder => {
                        editBuilder.insert(position, selected.label);
                    });
                }
            }
        })
    );

    // Commands
    context.subscriptions.push(
        vscode.commands.registerCommand('rules.validate', async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor || editor.document.languageId !== 'rules') {
                vscode.window.showWarningMessage('No active .rules file');
                return;
            }

            const content = editor.document.getText();
            try {
                const result = await cliClient.validateOnly(content);

                // Check validation result
                const hasErrors = result.validation?.syntax_errors?.length > 0;
                const hasWarnings = result.validation?.warnings?.length > 0;
                const hasInfo = result.validation?.info?.length > 0;

                if (result.success && result.validation?.syntax_valid) {
                    if (hasWarnings || hasInfo) {
                        const issueCount = (result.validation.warnings?.length || 0) + (result.validation.info?.length || 0);
                        vscode.window.showInformationMessage(`✅ Rule is syntactically valid (${issueCount} warnings/info)`);
                    } else {
                        vscode.window.showInformationMessage('✅ Rule is valid');
                    }
                } else if (hasErrors) {
                    const errorMsg = result.validation?.syntax_errors?.[0]?.message || 'Syntax errors detected';
                    vscode.window.showErrorMessage(`❌ Validation failed: ${errorMsg}`);
                } else {
                    vscode.window.showErrorMessage(`❌ Validation failed: ${result.error || 'Unknown error'}`);
                }
            } catch (error) {
                vscode.window.showErrorMessage(`❌ Validation error: ${error}`);
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('rules.generate', async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor || editor.document.languageId !== 'rules') {
                vscode.window.showWarningMessage('No active .rules file');
                return;
            }

            const ruleFilePath = editor.document.uri.fsPath;

            try {
                const result = await cliClient.generateCode(ruleFilePath);

                if (result.success && result.files) {
                    // Open the main Java file
                    const mainFilePath = result.files.production;
                    if (mainFilePath && require('fs').existsSync(mainFilePath)) {
                        const doc = await vscode.workspace.openTextDocument(mainFilePath);
                        await vscode.window.showTextDocument(doc, vscode.ViewColumn.Beside);
                        vscode.window.showInformationMessage(`✅ Java code generated: ${result.className}`);
                    } else {
                        vscode.window.showInformationMessage(`✅ ${result.artifactCount} files generated in ${result.outputDirectory}`);
                    }
                } else {
                    const errorMsg = result.error || 'Unknown error';
                    if (result.traceback) {
                        vscode.window.showErrorMessage(`❌ Generation failed: ${errorMsg}\n\nSee output for details`);
                        console.error('Generation traceback:', result.traceback);
                    } else {
                        vscode.window.showErrorMessage(`❌ Generation failed: ${errorMsg}`);
                    }
                }
            } catch (error) {
                vscode.window.showErrorMessage(`❌ Generation error: ${error}`);
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('rules.showBuildOutput', () => {
            buildOutputChannel.show(true);
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('rules.build', async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor || editor.document.languageId !== 'rules') {
                vscode.window.showWarningMessage('No active .rules file');
                return;
            }

            const fileName = vscode.workspace.asRelativePath(editor.document.uri);
            const filePath = editor.document.uri.fsPath;
            const startTime = Date.now();

            // Save file first to ensure CLI reads latest content
            await editor.document.save();

            // Clear previous output and show channel
            buildOutputChannel.clear();
            buildOutputChannel.show(true);

            // Write build header
            buildOutputChannel.appendLine(`${getTimestamp()} Building rule: ${fileName}`);
            buildOutputChannel.appendLine(`${getTimestamp()} Using CLI code generation (no backend required)`);

            // Update status bar
            buildStatusBar.text = '$(sync~spin) Building...';
            buildStatusBar.tooltip = 'Building rule...';

            try {
                buildOutputChannel.appendLine(`${getTimestamp()} Generating Java code...`);
                const genStartTime = Date.now();

                const result = await cliClient.generateCode(filePath);
                const genDuration = ((Date.now() - genStartTime) / 1000).toFixed(2);

                if (result.success) {
                    buildOutputChannel.appendLine(`${getTimestamp()} ✅ Code generation succeeded (${genDuration}s)`);
                    buildOutputChannel.appendLine('');
                    buildOutputChannel.appendLine(`${getTimestamp()} Generated Files:`);
                    buildOutputChannel.appendLine(`${getTimestamp()}   Rule name: ${result.ruleName}`);
                    buildOutputChannel.appendLine(`${getTimestamp()}   Class name: ${result.className}`);
                    buildOutputChannel.appendLine(`${getTimestamp()}   Output directory: ${result.outputDirectory}`);
                    buildOutputChannel.appendLine(`${getTimestamp()}   Files generated: ${result.artifactCount}`);

                    if (result.files) {
                        buildOutputChannel.appendLine(`${getTimestamp()}     - ${result.files.production}`);
                        buildOutputChannel.appendLine(`${getTimestamp()}     - ${result.files.test}`);
                    }

                    buildOutputChannel.appendLine('');
                    const totalDuration = ((Date.now() - startTime) / 1000).toFixed(2);
                    buildOutputChannel.appendLine(`${getTimestamp()} ✅ Build completed (${totalDuration}s)`);

                    buildStatusBar.text = '$(check) Build: Success';
                    buildStatusBar.tooltip = `Build completed in ${totalDuration}s - Click to show output`;
                    vscode.window.showInformationMessage(`✅ Build successful - Java code generated in ${result.outputDirectory}`);
                } else {
                    buildOutputChannel.appendLine(`${getTimestamp()} ❌ Code generation failed (${genDuration}s)`);
                    buildOutputChannel.appendLine(`${getTimestamp()}   Error: ${result.error || 'Unknown error'}`);

                    if (result.traceback) {
                        buildOutputChannel.appendLine(`${getTimestamp()}   Type: ${result.traceback}`);
                    }

                    buildStatusBar.text = '$(error) Build: Failed';
                    buildStatusBar.tooltip = 'Code generation failed - Click to show output';
                    vscode.window.showErrorMessage(`❌ Build failed: ${result.error}`);
                }

            } catch (error: any) {
                const duration = ((Date.now() - startTime) / 1000).toFixed(2);
                buildOutputChannel.appendLine('');
                buildOutputChannel.appendLine(`${getTimestamp()} ❌ Build Error (${duration}s)`);
                buildOutputChannel.appendLine(`${getTimestamp()}   ${error.message || error}`);

                buildStatusBar.text = '$(error) Build: Error';
                buildStatusBar.tooltip = 'Build failed - Click to show details';
                vscode.window.showErrorMessage(`❌ Build failed: ${error.message}`);
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('rules.attachContext', async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor || editor.document.languageId !== 'rules') {
                vscode.window.showWarningMessage('No active .rules file');
                return;
            }

            // Get available contexts
            const contexts = workspaceConfig.getAllContexts();
            const items: vscode.QuickPickItem[] = [];

            contexts.forEach((context, contextKey) => {
                items.push({
                    label: context.name,
                    description: contextKey,
                    detail: context.description
                });
            });

            const selected = await vscode.window.showQuickPick(items, {
                placeHolder: 'Select a test context'
            });

            if (selected && selected.description) {
                // Insert frontmatter with context
                const edit = new vscode.WorkspaceEdit();
                const firstLine = editor.document.lineAt(0);
                const frontmatter = `---\ncontext: ${selected.description}\n---\n\n`;

                edit.insert(editor.document.uri, new vscode.Position(0, 0), frontmatter);
                await vscode.workspace.applyEdit(edit);
                vscode.window.showInformationMessage(`✅ Attached context: ${selected.label}`);
            }
        })
    );

    // Help command
    context.subscriptions.push(
        vscode.commands.registerCommand('rules.showHelp', async () => {
            const panel = vscode.window.createWebviewPanel(
                'rulesHelp',
                'Rules DSL - Keyboard Shortcuts',
                vscode.ViewColumn.Beside,
                {}
            );

            panel.webview.html = `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Rules DSL Help</title>
    <style>
        body {
            font-family: var(--vscode-font-family);
            color: var(--vscode-foreground);
            background-color: var(--vscode-editor-background);
            padding: 20px;
            line-height: 1.6;
        }
        h1 {
            color: var(--vscode-textLink-foreground);
            border-bottom: 2px solid var(--vscode-textLink-foreground);
            padding-bottom: 10px;
        }
        h2 {
            color: var(--vscode-textLink-activeForeground);
            margin-top: 30px;
            margin-bottom: 15px;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
        }
        th {
            background-color: var(--vscode-editor-selectionBackground);
            padding: 10px;
            text-align: left;
            font-weight: bold;
        }
        td {
            padding: 10px;
            border-bottom: 1px solid var(--vscode-widget-border);
        }
        tr:hover {
            background-color: var(--vscode-list-hoverBackground);
        }
        kbd {
            background-color: var(--vscode-textCodeBlock-background);
            border: 1px solid var(--vscode-widget-border);
            border-radius: 3px;
            padding: 2px 6px;
            font-family: monospace;
            font-size: 0.9em;
        }
        ul {
            list-style-type: none;
            padding-left: 0;
        }
        li {
            padding: 5px 0;
            padding-left: 20px;
            position: relative;
        }
        li:before {
            content: "→";
            position: absolute;
            left: 0;
            color: var(--vscode-textLink-foreground);
        }
    </style>
</head>
<body>
    <h1>🎯 Rules DSL - Keyboard Shortcuts</h1>

    <h2>⌨️ Keyboard Shortcuts</h2>
    <p>All shortcuts use <kbd>Alt+R</kbd> prefix:</p>
    <table>
        <tr>
            <th>Shortcut</th>
            <th>Command</th>
            <th>Description</th>
        </tr>
        <tr>
            <td><kbd>Alt+R</kbd> <kbd>S</kbd></td>
            <td>Search</td>
            <td>Find attribute across all entities</td>
        </tr>
        <tr>
            <td><kbd>Alt+R</kbd> <kbd>C</kbd></td>
            <td>Context</td>
            <td>Attach test context to current rule</td>
        </tr>
        <tr>
            <td><kbd>Alt+R</kbd> <kbd>V</kbd></td>
            <td>Validate</td>
            <td>Validate rule with ANTLR parser</td>
        </tr>
        <tr>
            <td><kbd>Alt+R</kbd> <kbd>G</kbd></td>
            <td>Generate</td>
            <td>Generate Java code from rule</td>
        </tr>
        <tr>
            <td><kbd>Alt+R</kbd> <kbd>B</kbd></td>
            <td>Build</td>
            <td>Run semantic validation (undefined entities, cyclic calls, etc.)</td>
        </tr>
        <tr>
            <td><kbd>Alt+R</kbd> <kbd>R</kbd></td>
            <td>Refresh Schemas</td>
            <td>Reload entity schema definitions</td>
        </tr>
        <tr>
            <td><kbd>Alt+R</kbd> <kbd>T</kbd></td>
            <td>Refresh Contexts</td>
            <td>Reload test context data</td>
        </tr>
        <tr>
            <td><kbd>Alt+R</kbd> <kbd>H</kbd></td>
            <td>Help</td>
            <td>Show this keyboard shortcuts help</td>
        </tr>
    </table>

    <h2>📁 Tree Views</h2>
    <ul>
        <li>Click the <strong>"Rules Engine"</strong> icon in the activity bar (left sidebar)</li>
        <li>Expand <strong>Schemas</strong> to browse entity definitions</li>
        <li>Expand <strong>Test Contexts</strong> to view test data</li>
        <li>Click any attribute or field to insert at cursor position</li>
        <li>Click context name to attach as frontmatter to current rule</li>
    </ul>

    <h2>🎨 Command Palette</h2>
    <p>Press <kbd>Cmd+Shift+P</kbd> (Mac) or <kbd>Ctrl+Shift+P</kbd> (Windows/Linux) and type:</p>
    <ul>
        <li><strong>Rules: Validate Current Rule</strong> - Full ANTLR grammar validation</li>
        <li><strong>Rules: Build (Semantic Validation)</strong> - Deep validation with backend</li>
        <li><strong>Rules: Generate Java Code</strong> - Code generation from rule</li>
        <li><strong>Rules: Attach Test Context</strong> - Select and attach context</li>
        <li><strong>Rules: Find Attribute Across Entities</strong> - Cross-entity search</li>
        <li><strong>Rules: Show Keyboard Shortcuts</strong> - This help page</li>
    </ul>

    <h2>✨ Features</h2>
    <ul>
        <li><strong>Syntax Highlighting</strong> - Keywords, entities, operators, and values</li>
        <li><strong>Autocomplete</strong> - Context-aware suggestions for keywords, entities, and actions</li>
        <li><strong>Hover Documentation</strong> - Type and description info on hover</li>
        <li><strong>Live Diagnostics</strong> - Real-time validation for structural errors</li>
        <li><strong>Hierarchical Navigation</strong> - Tree views for schemas and test contexts</li>
    </ul>
</body>
</html>
            `;
        })
    );

    // Status bar
    const statusBar = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
    statusBar.text = "$(check) Rules DSL";
    statusBar.tooltip = "Rules DSL Extension Active";
    statusBar.show();
    context.subscriptions.push(statusBar);

    console.log('Rules DSL extension initialized successfully');
}

export function deactivate() {
    console.log('Rules DSL extension deactivated');
}
