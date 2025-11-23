import * as vscode from 'vscode';
import { WorkspaceConfig, EntitySchema } from '../workspace-config';

export class SchemaTreeDataProvider implements vscode.TreeDataProvider<SchemaTreeItem> {
    private _onDidChangeTreeData: vscode.EventEmitter<SchemaTreeItem | undefined | null | void> = new vscode.EventEmitter<SchemaTreeItem | undefined | null | void>();
    readonly onDidChangeTreeData: vscode.Event<SchemaTreeItem | undefined | null | void> = this._onDidChangeTreeData.event;

    constructor(private config: WorkspaceConfig) {}

    refresh(): void {
        this._onDidChangeTreeData.fire();
    }

    getTreeItem(element: SchemaTreeItem): vscode.TreeItem {
        return element;
    }

    getChildren(element?: SchemaTreeItem): Thenable<SchemaTreeItem[]> {
        if (!element) {
            // Root level - show all entities
            const schemas = this.config.getAllSchemas();
            const items: SchemaTreeItem[] = [];

            schemas.forEach((schema, entityName) => {
                items.push(new SchemaTreeItem(
                    entityName,
                    `${schema.attributes.length} attributes`,
                    vscode.TreeItemCollapsibleState.Collapsed,
                    'entity',
                    entityName
                ));
            });

            return Promise.resolve(items);
        } else if (element.contextValue === 'entity') {
            // Entity level - show all attributes
            const schema = this.config.getSchema(element.entityName!);
            if (!schema) {
                return Promise.resolve([]);
            }

            const items: SchemaTreeItem[] = schema.attributes.map(attr => {
                const required = attr.required ? ' (required)' : '';
                return new SchemaTreeItem(
                    attr.name,
                    `${attr.type}${required}`,
                    vscode.TreeItemCollapsibleState.None,
                    'attribute',
                    element.entityName,
                    attr.description
                );
            });

            return Promise.resolve(items);
        }

        return Promise.resolve([]);
    }
}

export class SchemaTreeItem extends vscode.TreeItem {
    constructor(
        public readonly label: string,
        public readonly description: string,
        public readonly collapsibleState: vscode.TreeItemCollapsibleState,
        public readonly contextValue: 'entity' | 'attribute',
        public readonly entityName?: string,
        public readonly tooltip?: string
    ) {
        super(label, collapsibleState);

        this.description = description;
        this.tooltip = tooltip || description;

        // Set icons
        if (contextValue === 'entity') {
            this.iconPath = new vscode.ThemeIcon('symbol-class');
        } else if (contextValue === 'attribute') {
            this.iconPath = new vscode.ThemeIcon('symbol-field');
        }

        // Make attributes clickable to insert
        if (contextValue === 'attribute') {
            this.command = {
                command: 'rules.insertAttribute',
                title: 'Insert Attribute',
                arguments: [entityName, label]
            };
        }
    }
}
