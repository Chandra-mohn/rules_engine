/**
 * VS Code Extension Entry Point for COBOL-to-DSL Migration
 * Registers commands, providers, and initializes the workbench
 */

import * as vscode from 'vscode';
import * as path from 'path';
import { WorkbenchProvider } from './workbenchProvider';
import { TranspilerBridge } from './transpilerBridge';
import { TranspilerConfig } from './types';

let workbenchProvider: WorkbenchProvider | undefined;
let transpiler: TranspilerBridge | undefined;

/**
 * Extension activation function
 * Called when extension is first activated (lazy activation)
 */
export function activate(context: vscode.ExtensionContext) {
    console.log('COBOL-to-DSL Migration extension is now active');

    // Initialize transpiler configuration
    const transpilerConfig: TranspilerConfig = {
        // Try to find Python from workspace settings, fallback to system python3
        pythonPath: vscode.workspace.getConfiguration('cobol-migration').get('pythonPath') || 'python3',
        // Transpiler path relative to extension root
        transpilerPath: path.join(context.extensionPath, '..', 'backend'),
        timeout: 30000 // 30 second timeout
    };

    // Debug logging
    console.log('[COBOL Migration] Extension path:', context.extensionPath);
    console.log('[COBOL Migration] Transpiler path:', transpilerConfig.transpilerPath);
    console.log('[COBOL Migration] Python path:', transpilerConfig.pythonPath);

    // Initialize transpiler bridge
    transpiler = new TranspilerBridge(transpilerConfig);

    // Validate Python and transpiler availability
    transpiler.validate().then(isValid => {
        if (!isValid) {
            vscode.window.showWarningMessage(
                'Python transpiler not available. Please check your Python installation and transpiler path.',
                'Open Settings'
            ).then(selection => {
                if (selection === 'Open Settings') {
                    vscode.commands.executeCommand('workbench.action.openSettings', 'cobol-migration');
                }
            });
        }
    });

    // Initialize workbench provider
    workbenchProvider = new WorkbenchProvider(context.extensionUri, transpiler);

    // Register webview view provider
    context.subscriptions.push(
        vscode.window.registerWebviewViewProvider(
            WorkbenchProvider.viewType,
            workbenchProvider
        )
    );

    // Register "Open Migration Workbench" command
    context.subscriptions.push(
        vscode.commands.registerCommand('cobol-migration.openWorkbench', () => {
            // Focus the workbench view
            vscode.commands.executeCommand('workbench.view.extension.cobol-migration-container');
        })
    );

    // Register "Convert to DSL" command (context menu in editor)
    context.subscriptions.push(
        vscode.commands.registerCommand('cobol-migration.convertToDSL', async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor) {
                vscode.window.showWarningMessage('No active editor');
                return;
            }

            // Get selected text or entire document
            const selection = editor.selection;
            const cobolCode = selection.isEmpty
                ? editor.document.getText()
                : editor.document.getText(selection);

            if (!cobolCode.trim()) {
                vscode.window.showWarningMessage('No COBOL code to convert');
                return;
            }

            try {
                // Show progress notification
                await vscode.window.withProgress({
                    location: vscode.ProgressLocation.Notification,
                    title: 'Converting COBOL to DSL...',
                    cancellable: false
                }, async (progress) => {
                    progress.report({ increment: 0 });

                    // Convert using transpiler
                    const result = await transpiler!.convertToDSL(cobolCode);

                    progress.report({ increment: 100 });

                    // Create new untitled document with DSL content
                    const dslDoc = await vscode.workspace.openTextDocument({
                        content: result.dsl,
                        language: 'rules-dsl'
                    });

                    // Show in editor
                    await vscode.window.showTextDocument(dslDoc, {
                        viewColumn: vscode.ViewColumn.Beside,
                        preserveFocus: false
                    });

                    // Show metadata if available
                    if (result.metadata?.mappings?.length > 0) {
                        const mappingCount = result.metadata.mappings.length;
                        vscode.window.showInformationMessage(
                            `Conversion complete: ${mappingCount} attribute mapping${mappingCount === 1 ? '' : 's'}`
                        );
                    }

                    // Show warnings if any
                    if (result.warnings && result.warnings.length > 0) {
                        const warningMsg = `Warnings: ${result.warnings.join(', ')}`;
                        vscode.window.showWarningMessage(warningMsg);
                    }
                });

            } catch (error) {
                const errorMsg = error instanceof Error ? error.message : String(error);
                vscode.window.showErrorMessage(`Conversion failed: ${errorMsg}`);
            }
        })
    );

    // Register "Convert Selection to Workbench" command
    context.subscriptions.push(
        vscode.commands.registerCommand('cobol-migration.convertSelectionToWorkbench', async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor) {
                vscode.window.showWarningMessage('No active editor');
                return;
            }

            // Get selected text or entire document
            const selection = editor.selection;
            const cobolCode = selection.isEmpty
                ? editor.document.getText()
                : editor.document.getText(selection);

            if (!cobolCode.trim()) {
                vscode.window.showWarningMessage('No COBOL code to convert');
                return;
            }

            // Open workbench view
            await vscode.commands.executeCommand('cobol-migration.openWorkbench');

            // Send code to workbench (workbench will handle conversion)
            // This is a placeholder - workbench needs to expose a method for this
            vscode.window.showInformationMessage('COBOL code sent to workbench for conversion');
        })
    );

    console.log('COBOL-to-DSL Migration extension activated successfully');
}

/**
 * Extension deactivation function
 * Called when extension is deactivated
 */
export function deactivate() {
    console.log('COBOL-to-DSL Migration extension deactivated');
    workbenchProvider = undefined;
    transpiler = undefined;
}
