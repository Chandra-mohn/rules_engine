import * as vscode from 'vscode';
import { WorkspaceConfig } from '../workspace-config';

export class RulesHoverProvider implements vscode.HoverProvider {
    constructor(private config: WorkspaceConfig) {}

    provideHover(
        document: vscode.TextDocument,
        position: vscode.Position
    ): vscode.Hover | undefined {
        const range = document.getWordRangeAtPosition(position);
        if (!range) {
            return undefined;
        }

        const word = document.getText(range);

        // Check if hovering over entity field
        const line = document.lineAt(position.line).text;
        const entityMatch = line.match(/(\w+)\.(\w+)/);

        if (entityMatch) {
            const entityName = entityMatch[1];
            const fieldName = entityMatch[2];

            if (word === fieldName) {
                return this.getFieldHover(entityName, fieldName);
            }

            if (word === entityName) {
                return this.getEntityHover(entityName);
            }
        }

        // Check if hovering over keyword
        return this.getKeywordHover(word);
    }

    private getFieldHover(entityName: string, fieldName: string): vscode.Hover | undefined {
        const schema = this.config.getSchema(entityName);
        if (!schema) {
            return undefined;
        }

        const field = schema.attributes.find(attr => attr.name === fieldName);
        if (!field) {
            return undefined;
        }

        const markdown = new vscode.MarkdownString();
        markdown.appendCodeblock(field.name, 'rules');
        markdown.appendMarkdown(`**Type:** \`${field.type}\`\n\n`);
        if (field.description) {
            markdown.appendMarkdown(field.description);
        }
        if (field.required !== undefined) {
            markdown.appendMarkdown(`\n\n**Required:** ${field.required ? 'Yes' : 'No'}`);
        }

        return new vscode.Hover(markdown);
    }

    private getEntityHover(entityName: string): vscode.Hover | undefined {
        const schema = this.config.getSchema(entityName);
        if (!schema) {
            return undefined;
        }

        const markdown = new vscode.MarkdownString();
        markdown.appendMarkdown(`**Entity:** \`${entityName}\`\n\n`);
        markdown.appendMarkdown(`**Fields:**\n`);
        schema.attributes.forEach(attr => {
            markdown.appendMarkdown(`- \`${attr.name}\` (${attr.type})\n`);
        });

        return new vscode.Hover(markdown);
    }

    private getKeywordHover(word: string): vscode.Hover | undefined {
        const keywords: Record<string, string> = {
            'rule': 'Define a new rule with conditions and actions',
            'actionset': 'Define a set of actions to be executed together',
            'if': 'Conditional statement - evaluates a condition',
            'then': 'Action to execute when condition is true',
            'else': 'Action to execute when condition is false',
            'and': 'Logical AND operator - both conditions must be true',
            'or': 'Logical OR operator - at least one condition must be true',
            'not': 'Logical NOT operator - negates the condition',
            'with': 'Specify additional parameters for an action'
        };

        const description = keywords[word.toLowerCase()];
        if (!description) {
            return undefined;
        }

        const markdown = new vscode.MarkdownString();
        markdown.appendCodeblock(word, 'rules');
        markdown.appendMarkdown(description);

        return new vscode.Hover(markdown);
    }
}
