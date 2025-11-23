import * as vscode from 'vscode';
import * as yaml from 'js-yaml';
import * as fs from 'fs';
import * as path from 'path';

export interface EntitySchema {
    name: string;
    attributes: Array<{
        name: string;
        type: string;
        description?: string;
        required?: boolean;
    }>;
}

export interface TestContext {
    name: string;
    description?: string;
    is_schema_template: boolean;
    version?: string;
    client_code?: string;
    context_data: Record<string, any>;
}

export class WorkspaceConfig {
    private schemas: Map<string, EntitySchema> = new Map();
    private contexts: Map<string, TestContext> = new Map();
    private config: any = null;

    load() {
        const workspaceFolders = vscode.workspace.workspaceFolders;
        if (!workspaceFolders || workspaceFolders.length === 0) {
            console.warn('No workspace folder found');
            return;
        }

        const workspaceRoot = workspaceFolders[0].uri.fsPath;

        // Load rules.config.yaml
        const configPath = path.join(workspaceRoot, 'rules.config.yaml');
        if (fs.existsSync(configPath)) {
            const configContent = fs.readFileSync(configPath, 'utf8');
            this.config = yaml.load(configContent);
            console.log('Loaded workspace config:', this.config);
        }

        // Load entity schemas
        this.loadSchemas(workspaceRoot);

        // Load contexts
        this.loadContexts(workspaceRoot);
    }

    private loadSchemas(workspaceRoot: string) {
        const schemasPath = path.join(workspaceRoot, 'rules', 'schemas');
        if (!fs.existsSync(schemasPath)) {
            console.warn('Schemas directory not found');
            return;
        }

        const schemaFiles = fs.readdirSync(schemasPath).filter(f => f.endsWith('.json') && f !== 'schema.json');

        for (const file of schemaFiles) {
            try {
                const filePath = path.join(schemasPath, file);
                const content = fs.readFileSync(filePath, 'utf8');
                const schema = JSON.parse(content);

                const entityName = path.basename(file, '.json');
                this.schemas.set(entityName, {
                    name: entityName,
                    attributes: schema.attributes || []
                });

                console.log(`Loaded schema: ${entityName} with ${schema.attributes?.length || 0} attributes`);
            } catch (error) {
                console.error(`Failed to load schema ${file}:`, error);
            }
        }
    }

    private loadContexts(workspaceRoot: string) {
        const contextsPath = path.join(workspaceRoot, 'rules', 'contexts');
        if (!fs.existsSync(contextsPath)) {
            console.warn('Contexts directory not found');
            return;
        }

        // Load regular contexts
        const contextFiles = fs.readdirSync(contextsPath).filter(f => f.endsWith('.json'));
        for (const file of contextFiles) {
            try {
                const filePath = path.join(contextsPath, file);
                const content = fs.readFileSync(filePath, 'utf8');
                const context = JSON.parse(content);

                const contextKey = path.basename(file, '.json');
                this.contexts.set(contextKey, context);

                console.log(`Loaded context: ${context.name}`);
            } catch (error) {
                console.error(`Failed to load context ${file}:`, error);
            }
        }

        // Load schema templates
        const templatesPath = path.join(contextsPath, 'schema-templates');
        if (fs.existsSync(templatesPath)) {
            const templateFiles = fs.readdirSync(templatesPath).filter(f => f.endsWith('.json'));
            for (const file of templateFiles) {
                try {
                    const filePath = path.join(templatesPath, file);
                    const content = fs.readFileSync(filePath, 'utf8');
                    const template = JSON.parse(content);

                    const templateKey = 'template-' + path.basename(file, '.json');
                    this.contexts.set(templateKey, template);

                    console.log(`Loaded template: ${template.name}`);
                } catch (error) {
                    console.error(`Failed to load template ${file}:`, error);
                }
            }
        }

        console.log(`Loaded ${this.contexts.size} contexts`);
    }

    getSchema(entityName: string): EntitySchema | undefined {
        return this.schemas.get(entityName);
    }

    getAllSchemas(): Map<string, EntitySchema> {
        return this.schemas;
    }

    getContext(contextKey: string): TestContext | undefined {
        return this.contexts.get(contextKey);
    }

    getAllContexts(): Map<string, TestContext> {
        return this.contexts;
    }

    getConfig(): any {
        return this.config;
    }
}
