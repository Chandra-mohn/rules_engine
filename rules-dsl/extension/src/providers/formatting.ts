import * as vscode from 'vscode';

/**
 * Provides document formatting for Rules DSL files.
 * Handles indentation for if/then/else/endif blocks.
 */
export class RulesFormattingProvider implements vscode.DocumentFormattingEditProvider {
    private readonly indentSize = 4;

    provideDocumentFormattingEdits(
        document: vscode.TextDocument
    ): vscode.TextEdit[] {
        const edits: vscode.TextEdit[] = [];
        let indentLevel = 0;
        let inFrontmatter = false;

        for (let i = 0; i < document.lineCount; i++) {
            const line = document.lineAt(i);
            const trimmed = line.text.trim();

            // Skip empty lines
            if (trimmed === '') {
                continue;
            }

            // Track frontmatter
            if (trimmed === '---') {
                inFrontmatter = !inFrontmatter;
                continue;
            }

            // Don't indent frontmatter content
            if (inFrontmatter) {
                continue;
            }

            // Skip comments
            if (trimmed.startsWith('//')) {
                continue;
            }

            // Calculate indent for this line
            let currentIndent = indentLevel;

            // Decrease indent for endif, else, elseif (before we process the line)
            if (trimmed.startsWith('endif') || trimmed.startsWith('else') || trimmed.startsWith('elseif')) {
                currentIndent = Math.max(0, indentLevel - 1);
            }

            // Build correctly indented line
            const targetIndent = ' '.repeat(currentIndent * this.indentSize);
            const expectedLine = targetIndent + trimmed;

            // Only create edit if line needs fixing
            if (line.text !== expectedLine) {
                const range = new vscode.Range(
                    new vscode.Position(i, 0),
                    new vscode.Position(i, line.text.length)
                );
                edits.push(vscode.TextEdit.replace(range, expectedLine));
            }

            // Adjust indent level for next line
            if (trimmed.startsWith('if ') || trimmed.startsWith('elseif ')) {
                // Check if there's a "then" on the same line
                if (trimmed.includes(' then')) {
                    indentLevel++;
                }
            } else if (trimmed.startsWith('rule ') || trimmed.startsWith('actionset ')) {
                indentLevel++;
            } else if (trimmed.startsWith('endif')) {
                indentLevel = Math.max(0, indentLevel - 1);
            }
        }

        return edits;
    }
}
