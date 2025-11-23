import * as vscode from 'vscode';
import { WorkspaceConfig, TestContext } from '../workspace-config';

export class ContextTreeDataProvider implements vscode.TreeDataProvider<ContextTreeItem> {
    private _onDidChangeTreeData: vscode.EventEmitter<ContextTreeItem | undefined | null | void> = new vscode.EventEmitter<ContextTreeItem | undefined | null | void>();
    readonly onDidChangeTreeData: vscode.Event<ContextTreeItem | undefined | null | void> = this._onDidChangeTreeData.event;

    constructor(private config: WorkspaceConfig) {}

    refresh(): void {
        this._onDidChangeTreeData.fire();
    }

    getTreeItem(element: ContextTreeItem): vscode.TreeItem {
        return element;
    }

    getChildren(element?: ContextTreeItem): Thenable<ContextTreeItem[]> {
        if (!element) {
            // Root level - show all contexts
            const contexts = this.config.getAllContexts();
            const items: ContextTreeItem[] = [];

            contexts.forEach((context, contextKey) => {
                const entityCount = Object.keys(context.context_data || {}).length;
                items.push(new ContextTreeItem(
                    context.name,
                    `${entityCount} entities`,
                    vscode.TreeItemCollapsibleState.Collapsed,
                    'context',
                    contextKey,
                    context.description,
                    context.is_schema_template
                ));
            });

            return Promise.resolve(items);
        } else if (element.contextValue === 'context') {
            // Context level - show all entities in context_data
            const context = this.config.getContext(element.contextKey!);
            if (!context || !context.context_data) {
                return Promise.resolve([]);
            }

            const items: ContextTreeItem[] = [];
            for (const [entityName, entityData] of Object.entries(context.context_data)) {
                const fieldCount = Object.keys(entityData as object).filter(k => !k.startsWith('_')).length;
                items.push(new ContextTreeItem(
                    entityName,
                    `${fieldCount} fields`,
                    vscode.TreeItemCollapsibleState.Collapsed,
                    'entity',
                    element.contextKey,
                    undefined,
                    element.isTemplate,
                    entityName
                ));
            }

            return Promise.resolve(items);
        } else if (element.contextValue === 'entity') {
            // Entity level - show all field values
            const context = this.config.getContext(element.contextKey!);
            if (!context || !context.context_data || !element.entityName) {
                return Promise.resolve([]);
            }

            const entityData = context.context_data[element.entityName] as Record<string, any>;
            if (!entityData) {
                return Promise.resolve([]);
            }

            const items: ContextTreeItem[] = [];
            for (const [fieldName, fieldValue] of Object.entries(entityData)) {
                // Skip metadata fields
                if (fieldName.startsWith('_')) {
                    continue;
                }

                const valueStr = this.formatValue(fieldValue);
                items.push(new ContextTreeItem(
                    fieldName,
                    valueStr,
                    vscode.TreeItemCollapsibleState.None,
                    'field',
                    element.contextKey,
                    `${fieldName}: ${valueStr}`,
                    element.isTemplate,
                    element.entityName,
                    fieldName
                ));
            }

            return Promise.resolve(items);
        }

        return Promise.resolve([]);
    }

    private formatValue(value: any): string {
        if (value === null || value === undefined) {
            return 'null';
        }
        if (typeof value === 'string') {
            return `"${value}"`;
        }
        if (typeof value === 'number' || typeof value === 'boolean') {
            return String(value);
        }
        if (Array.isArray(value)) {
            return `[${value.length} items]`;
        }
        if (typeof value === 'object') {
            return `{${Object.keys(value).length} fields}`;
        }
        return String(value);
    }
}

export class ContextTreeItem extends vscode.TreeItem {
    constructor(
        public readonly label: string,
        public readonly description: string,
        public readonly collapsibleState: vscode.TreeItemCollapsibleState,
        public readonly contextValue: 'context' | 'entity' | 'field',
        public readonly contextKey?: string,
        public readonly tooltip?: string,
        public readonly isTemplate?: boolean,
        public readonly entityName?: string,
        public readonly fieldName?: string
    ) {
        super(label, collapsibleState);

        this.description = description;
        this.tooltip = tooltip || description;

        // Set icons
        if (contextValue === 'context') {
            this.iconPath = isTemplate
                ? new vscode.ThemeIcon('symbol-interface')
                : new vscode.ThemeIcon('beaker');
        } else if (contextValue === 'entity') {
            this.iconPath = new vscode.ThemeIcon('symbol-class');
        } else if (contextValue === 'field') {
            this.iconPath = new vscode.ThemeIcon('symbol-field');
        }

        // Make fields clickable to insert value
        if (contextValue === 'field' && entityName && fieldName) {
            this.command = {
                command: 'rules.insertContextField',
                title: 'Insert Field',
                arguments: [entityName, fieldName]
            };
        }

        // Make contexts clickable to attach (for non-templates)
        if (contextValue === 'context' && !isTemplate && contextKey) {
            this.command = {
                command: 'rules.attachContextFromTree',
                title: 'Attach Context',
                arguments: [contextKey]
            };
        }
    }
}
