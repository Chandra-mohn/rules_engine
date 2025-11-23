import * as vscode from 'vscode';
import { WorkspaceConfig } from '../workspace-config';

export class RulesCompletionProvider implements vscode.CompletionItemProvider {
    constructor(private config: WorkspaceConfig) {}

    provideCompletionItems(
        document: vscode.TextDocument,
        position: vscode.Position
    ): vscode.CompletionItem[] {
        const line = document.lineAt(position.line).text;
        const prefix = line.substring(0, position.character);

        // Nested entity field autocomplete: "applicant.address.city"
        // Matches patterns like: "entity.", "entity.field.", "entity.field.subfield."
        const nestedMatch = prefix.match(/([\w.]+)\.$/);
        if (nestedMatch) {
            const path = nestedMatch[1];
            return this.getNestedFieldCompletions(path);
        }

        // Analyze structural context (scan previous lines to understand where we are)
        const context = this.analyzeStructuralContext(document, position);

        // Inside then/else block after actions, suggest completions based on block type
        if (context.inBlock && context.hasActions) {
            return this.getBlockCompletions(context.blockType);
        }

        // After "then <action>" or "else <action>" on same line
        if (this.isAfterAction(prefix)) {
            return this.getPostActionCompletions(prefix);
        }

        // Action autocomplete (immediately after "then" or "else")
        if (this.isActionContext(prefix)) {
            return this.getActionCompletions();
        }

        // Keyword autocomplete (but filter based on context)
        if (this.isKeywordContext(prefix)) {
            return this.getContextualKeywordCompletions(prefix);
        }

        return [];
    }

    private getNestedFieldCompletions(path: string): vscode.CompletionItem[] {
        const parts = path.split('.');

        // First part is the entity name
        const entityName = parts[0];

        // Try to infer structure from test context data
        const contextData = this.inferFieldsFromContext(entityName, parts.slice(1));
        if (contextData.length > 0) {
            return contextData;
        }

        // Fallback: use schema if available
        const schema = this.config.getSchema(entityName);
        if (!schema) {
            return [];
        }

        // If only entity name (no nested path yet), return top-level attributes
        if (parts.length === 1) {
            return schema.attributes.map(attr => {
                const item = new vscode.CompletionItem(attr.name, vscode.CompletionItemKind.Property);
                item.detail = `(${attr.type})`;
                item.documentation = new vscode.MarkdownString(
                    attr.description || `Field: ${attr.name}\nType: ${attr.type}`
                );
                item.insertText = attr.name;

                // Add a note if this is likely a complex object
                if (attr.type === 'object' || attr.type === 'array') {
                    item.detail += ' (nested)';
                }

                return item;
            });
        }

        // For nested paths without context data, return empty
        // (true nested support requires context data)
        return [];
    }

    private inferFieldsFromContext(entityName: string, nestedPath: string[]): vscode.CompletionItem[] {
        // Get all test contexts
        const allContexts = this.config.getAllContexts();

        // Navigate to the nested object using the path
        const fields = new Set<string>();

        for (const [_, context] of allContexts) {
            // Check if this context has the entity we're looking for
            if (!context.context_data || !context.context_data[entityName]) {
                continue;
            }

            // Navigate the nested path in the entity data
            let current: any = context.context_data[entityName];

            for (const pathPart of nestedPath) {
                if (!current || typeof current !== 'object') {
                    break;
                }

                // Handle arrays: if current is array, look at first element
                if (Array.isArray(current)) {
                    current = current[0];
                }

                current = current[pathPart];
            }

            // If we successfully navigated, collect field names from current object
            if (current && typeof current === 'object' && !Array.isArray(current)) {
                Object.keys(current).forEach(key => fields.add(key));
            } else if (Array.isArray(current) && current.length > 0 && typeof current[0] === 'object') {
                // If it's an array of objects, get keys from first element
                Object.keys(current[0]).forEach(key => fields.add(key));
            }
        }

        // Convert fields to completion items
        return Array.from(fields).map(fieldName => {
            const item = new vscode.CompletionItem(fieldName, vscode.CompletionItemKind.Property);
            item.detail = '(inferred from context)';
            item.documentation = new vscode.MarkdownString(
                `Field discovered in test context data`
            );
            item.insertText = fieldName;
            return item;
        });
    }

    private getKeywordCompletions(): vscode.CompletionItem[] {
        const keywords = [
            { label: 'rule', kind: vscode.CompletionItemKind.Keyword, detail: 'Define a new rule' },
            { label: 'actionset', kind: vscode.CompletionItemKind.Keyword, detail: 'Define an action set' },
            { label: 'if', kind: vscode.CompletionItemKind.Keyword, detail: 'Conditional statement' },
            { label: 'then', kind: vscode.CompletionItemKind.Keyword, detail: 'Then clause' },
            { label: 'elseif', kind: vscode.CompletionItemKind.Keyword, detail: 'Else if statement' },
            { label: 'else', kind: vscode.CompletionItemKind.Keyword, detail: 'Else clause' },
            { label: 'endif', kind: vscode.CompletionItemKind.Keyword, detail: 'End if statement' },
            { label: 'return', kind: vscode.CompletionItemKind.Keyword, detail: 'Return statement' },
            { label: 'and', kind: vscode.CompletionItemKind.Operator, detail: 'Logical AND' },
            { label: 'or', kind: vscode.CompletionItemKind.Operator, detail: 'Logical OR' },
            { label: 'not', kind: vscode.CompletionItemKind.Operator, detail: 'Logical NOT' },
            { label: 'in', kind: vscode.CompletionItemKind.Operator, detail: 'In operator (list membership)' },
            { label: 'not_in', kind: vscode.CompletionItemKind.Operator, detail: 'Not in operator' },
            { label: 'is_null', kind: vscode.CompletionItemKind.Operator, detail: 'Is null check' },
            { label: 'is_not_null', kind: vscode.CompletionItemKind.Operator, detail: 'Is not null check' },
            { label: 'contains', kind: vscode.CompletionItemKind.Operator, detail: 'Contains operator' },
            { label: 'starts_with', kind: vscode.CompletionItemKind.Operator, detail: 'Starts with operator' },
            { label: 'ends_with', kind: vscode.CompletionItemKind.Operator, detail: 'Ends with operator' },
            { label: 'matches', kind: vscode.CompletionItemKind.Operator, detail: 'Regex match operator' },
            { label: 'true', kind: vscode.CompletionItemKind.Value, detail: 'Boolean true' },
            { label: 'false', kind: vscode.CompletionItemKind.Value, detail: 'Boolean false' },
            { label: 'null', kind: vscode.CompletionItemKind.Value, detail: 'Null value' }
        ];

        return keywords.map(kw => {
            const item = new vscode.CompletionItem(kw.label, kw.kind);
            item.detail = kw.detail;
            return item;
        });
    }

    private getActionCompletions(): vscode.CompletionItem[] {
        const actions = [
            'approveApplication',
            'rejectApplication',
            'sendEmail',
            'createAccount',
            'issueCard',
            'calculateLimit',
            'checkFraud'
        ];

        return actions.map(action => {
            const item = new vscode.CompletionItem(action, vscode.CompletionItemKind.Function);
            item.detail = 'Action';
            return item;
        });
    }

    private isKeywordContext(prefix: string): boolean {
        // Suggest keywords in various contexts
        const trimmed = prefix.trim();

        // Don't suggest keywords right after complete "if" (user needs to write condition first)
        if (/\bif\s+$/i.test(trimmed)) {
            return false;
        }

        // Don't suggest after "then" or "else" keywords (those contexts handled elsewhere)
        if (/\b(then|else)\s+$/i.test(trimmed)) {
            return false;
        }

        // Suggest keywords in statement positions:
        // - Empty/whitespace only
        // - After flow control keywords (then, else, endif) + space
        // - Partial keyword typing (e.g., user typed "i" for "if")
        return trimmed.length === 0 ||
               /^\s*$/.test(prefix) ||
               /\b(then|else|endif)\s+$/i.test(trimmed) ||
               /^[a-z_]+$/i.test(trimmed); // Partial keyword (letters/underscore only)
    }

    private isActionContext(prefix: string): boolean {
        // Immediately after "then" or "else" keyword (no action yet)
        return /\b(then|else)\s+$/i.test(prefix.trim());
    }

    private isAfterAction(prefix: string): boolean {
        // After "then <action>" or "else <action>" (action already typed)
        const trimmed = prefix.trim();
        return /\b(then|else)\s+\w+\s*\(.*\)\s*$/i.test(trimmed) || // Action with parens
               /\b(then|else)\s+\w+\s*,\s*$/i.test(trimmed);         // After comma in action list
    }

    private getPostActionCompletions(prefix: string): vscode.CompletionItem[] {
        const trimmed = prefix.trim();
        const allKeywords = this.getKeywordCompletions();
        const actions = this.getActionCompletions();

        // Helper to get label as string
        const getLabel = (item: vscode.CompletionItem): string => {
            return typeof item.label === 'string' ? item.label : item.label.label;
        };

        // After "then <action>", can have: more actions (comma-separated), elseif, else, endif
        if (/\bthen\s+\w+/i.test(trimmed)) {
            const flowKeywords = allKeywords.filter(kw =>
                ['elseif', 'else', 'endif'].includes(getLabel(kw))
            );
            return [...actions, ...flowKeywords];
        }

        // After "else <action>", can have: more actions (comma-separated), endif ONLY
        if (/\belse\s+\w+/i.test(trimmed)) {
            const endifKeyword = allKeywords.filter(kw => getLabel(kw) === 'endif');
            return [...actions, ...endifKeyword];
        }

        return [];
    }

    private getContextualKeywordCompletions(prefix: string): vscode.CompletionItem[] {
        const trimmed = prefix.trim();
        const allKeywords = this.getKeywordCompletions();

        // Helper to get label as string
        const getLabel = (item: vscode.CompletionItem): string => {
            return typeof item.label === 'string' ? item.label : item.label.label;
        };

        // After "if <condition>", suggest "then"
        if (/\bif\s+.+$/i.test(trimmed) && !/\bthen\b/i.test(trimmed)) {
            return allKeywords.filter(kw => getLabel(kw) === 'then');
        }

        // After "then <action>", suggest "elseif", "else", "endif"
        if (/\bthen\s+\w+/i.test(trimmed)) {
            return allKeywords.filter(kw => ['elseif', 'else', 'endif'].includes(getLabel(kw)));
        }

        // After "else <action>", suggest "endif"
        if (/\belse\s+\w+/i.test(trimmed)) {
            return allKeywords.filter(kw => getLabel(kw) === 'endif');
        }

        // Default: return all keywords
        return allKeywords;
    }

    private analyzeStructuralContext(document: vscode.TextDocument, position: vscode.Position): {
        inBlock: boolean;
        blockType: 'then' | 'else' | null;
        hasActions: boolean;
    } {
        // Scan backwards from current line to find context
        let inThenBlock = false;
        let inElseBlock = false;
        let hasActions = false;
        let depth = 0; // Track nesting depth (0 = our block, >0 = inside nested blocks)

        for (let i = position.line - 1; i >= 0; i--) {
            const lineText = document.lineAt(i).text.trim();

            // Skip empty lines and comments
            if (lineText === '' || lineText.startsWith('//')) {
                continue;
            }

            // Check for endif - entering a completed nested block
            if (/\bendif\b/i.test(lineText)) {
                depth++;
                continue;
            }

            // Check for if/elseif - exiting to outer block or found our block's opening
            if (/\b(if|elseif)\b/i.test(lineText)) {
                if (depth === 0) {
                    // Found our block's opening if/elseif - stop scanning
                    break;
                } else {
                    // Exiting a nested block
                    depth--;
                    continue;
                }
            }

            // Only analyze at depth 0 (our current block, not nested blocks)
            if (depth === 0) {
                // Check for then (we're in then block)
                if (/\bthen\b/i.test(lineText)) {
                    inThenBlock = true;
                    // Check if there's an action on the same line
                    if (/\bthen\s+\w+\s*\(/i.test(lineText)) {
                        hasActions = true;
                    }
                    break;
                }

                // Check for else (we're in else block)
                if (/\belse\b/i.test(lineText)) {
                    inElseBlock = true;
                    // Check if there's an action on the same line
                    if (/\belse\s+\w+\s*\(/i.test(lineText)) {
                        hasActions = true;
                    }
                    break;
                }

                // Check if we have actions in current block (function call pattern)
                if (/\w+\s*\([^)]*\)/.test(lineText)) {
                    hasActions = true;
                }
            }
        }

        return {
            inBlock: inThenBlock || inElseBlock,
            blockType: inThenBlock ? 'then' : inElseBlock ? 'else' : null,
            hasActions
        };
    }

    private getBlockCompletions(blockType: 'then' | 'else' | null): vscode.CompletionItem[] {
        const allKeywords = this.getKeywordCompletions();
        const actions = this.getActionCompletions();

        // Helper to get label as string
        const getLabel = (item: vscode.CompletionItem): string => {
            return typeof item.label === 'string' ? item.label : item.label.label;
        };

        if (blockType === 'then') {
            // In then block: suggest actions + statement keywords (if, return) + flow control (elseif, else, endif)
            const relevantKeywords = allKeywords.filter(kw => {
                const label = getLabel(kw);
                return ['if', 'return', 'elseif', 'else', 'endif'].includes(label);
            });
            return [...actions, ...relevantKeywords];
        } else if (blockType === 'else') {
            // In else block: suggest actions + statement keywords (if, return) + endif ONLY
            const relevantKeywords = allKeywords.filter(kw => {
                const label = getLabel(kw);
                return ['if', 'return', 'endif'].includes(label);
            });
            return [...actions, ...relevantKeywords];
        }

        return [];
    }
}
