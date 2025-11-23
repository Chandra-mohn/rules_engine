import * as vscode from 'vscode';

export interface SemanticError {
    type: string;
    message: string;
    line: number;
    column: number;
    severity: string;
    suggestion?: string;
}

export class RulesDiagnosticProvider {
    private diagnosticCollection: vscode.DiagnosticCollection;

    constructor() {
        this.diagnosticCollection = vscode.languages.createDiagnosticCollection('rules');
    }

    public updateDiagnostics(document: vscode.TextDocument): void {
        if (document.languageId !== 'rules') {
            return;
        }

        const diagnostics: vscode.Diagnostic[] = [];

        // Validate if/endif matching
        this.validateIfEndifMatching(document, diagnostics);

        // Validate rule structure
        this.validateRuleStructure(document, diagnostics);

        this.diagnosticCollection.set(document.uri, diagnostics);
    }

    public updateSemanticDiagnostics(document: vscode.TextDocument, errors: SemanticError[], warnings: SemanticError[]): void {
        if (document.languageId !== 'rules') {
            return;
        }

        const diagnostics: vscode.Diagnostic[] = [];

        // First add local validation errors
        this.validateIfEndifMatching(document, diagnostics);
        this.validateRuleStructure(document, diagnostics);

        // Add semantic errors from backend
        for (const error of errors) {
            const line = Math.max(0, error.line - 1); // Convert to 0-based
            const column = Math.max(0, error.column - 1);
            const range = new vscode.Range(line, column, line, column + 10);

            let message = error.message;
            if (error.suggestion) {
                message += `\n💡 ${error.suggestion}`;
            }

            const diagnostic = new vscode.Diagnostic(
                range,
                message,
                error.severity === 'error' ? vscode.DiagnosticSeverity.Error : vscode.DiagnosticSeverity.Warning
            );
            diagnostic.source = 'Rules DSL (Backend)';
            diagnostic.code = error.type;
            diagnostics.push(diagnostic);
        }

        // Add semantic warnings from backend
        for (const warning of warnings) {
            const line = Math.max(0, warning.line - 1);
            const column = Math.max(0, warning.column - 1);
            const range = new vscode.Range(line, column, line, column + 10);

            let message = warning.message;
            if (warning.suggestion) {
                message += `\n💡 ${warning.suggestion}`;
            }

            const diagnostic = new vscode.Diagnostic(
                range,
                message,
                vscode.DiagnosticSeverity.Warning
            );
            diagnostic.source = 'Rules DSL (Backend)';
            diagnostic.code = warning.type;
            diagnostics.push(diagnostic);
        }

        this.diagnosticCollection.set(document.uri, diagnostics);
    }

    public clear(): void {
        this.diagnosticCollection.clear();
    }

    public dispose(): void {
        this.diagnosticCollection.dispose();
    }

    private validateIfEndifMatching(document: vscode.TextDocument, diagnostics: vscode.Diagnostic[]): void {
        const ifStack: { line: number; keyword: 'if' | 'elseif' }[] = [];
        const text = document.getText();
        const lines = text.split('\n');

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const trimmed = line.trim();

            // Skip empty lines and comments
            if (trimmed === '' || trimmed.startsWith('//')) {
                continue;
            }

            // Skip frontmatter
            if (trimmed === '---') {
                continue;
            }

            // Check for if/elseif (opening)
            if (/^\s*(if|elseif)\b/i.test(line)) {
                const keyword = /^\s*if\b/i.test(line) ? 'if' : 'elseif';
                ifStack.push({ line: i, keyword });
            }

            // Check for endif (closing)
            if (/\bendif\b/i.test(line)) {
                if (ifStack.length === 0) {
                    // Unmatched endif
                    const range = new vscode.Range(i, 0, i, line.length);
                    const diagnostic = new vscode.Diagnostic(
                        range,
                        'Unmatched "endif" - no corresponding "if" statement',
                        vscode.DiagnosticSeverity.Error
                    );
                    diagnostics.push(diagnostic);
                } else {
                    // Pop the matching if/elseif
                    ifStack.pop();
                }
            }
        }

        // Check for unclosed if statements
        for (const unclosed of ifStack) {
            const line = lines[unclosed.line];
            const range = new vscode.Range(unclosed.line, 0, unclosed.line, line.length);
            const diagnostic = new vscode.Diagnostic(
                range,
                `Missing "endif" for "${unclosed.keyword}" statement`,
                vscode.DiagnosticSeverity.Error
            );
            diagnostics.push(diagnostic);
        }
    }

    private validateRuleStructure(document: vscode.TextDocument, diagnostics: vscode.Diagnostic[]): void {
        const text = document.getText();
        const lines = text.split('\n');

        let hasRuleKeyword = false;
        let inFrontmatter = false;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const trimmed = line.trim();

            // Track frontmatter
            if (trimmed === '---') {
                inFrontmatter = !inFrontmatter;
                continue;
            }

            // Skip frontmatter content
            if (inFrontmatter) {
                continue;
            }

            // Skip empty lines and comments
            if (trimmed === '' || trimmed.startsWith('//')) {
                continue;
            }

            // Check for rule keyword
            if (/^\s*rule\s+/i.test(line)) {
                hasRuleKeyword = true;
                break;
            }

            // Check for actionset keyword
            if (/^\s*actionset\s+/i.test(line)) {
                hasRuleKeyword = true;
                break;
            }
        }

        // Warn if no rule/actionset definition found
        if (!hasRuleKeyword && lines.length > 3) {
            const range = new vscode.Range(0, 0, 0, 0);
            const diagnostic = new vscode.Diagnostic(
                range,
                'Missing "rule" or "actionset" definition',
                vscode.DiagnosticSeverity.Warning
            );
            diagnostics.push(diagnostic);
        }
    }
}
