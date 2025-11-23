import * as vscode from 'vscode';
import { spawn } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';

export interface CLIResult {
    success: boolean;
    ruleName?: string;
    className?: string;
    outputDirectory?: string;
    files?: {
        production: string;
        test: string;
    };
    artifactCount?: number;
    error?: string;
    traceback?: string;
}

export interface ValidationResult {
    success: boolean;
    valid?: boolean;
    syntax_valid?: boolean;
    syntax_errors?: Array<{
        type: string;
        line: number;
        column?: number;
        message: string;
        severity: string;
    }>;
    warnings?: Array<{
        type: string;
        message: string;
        severity: string;
    }>;
    info?: Array<{
        type: string;
        message: string;
        severity: string;
    }>;
    error?: string;
    validation?: any;
}

export class CLIClient {
    private pythonPath: string;
    private cliScriptPath: string;
    private extensionContext?: vscode.ExtensionContext;

    constructor(extensionContext?: vscode.ExtensionContext) {
        this.extensionContext = extensionContext;

        // Get Python path from configuration or use default
        this.pythonPath = vscode.workspace.getConfiguration('rules').get<string>('python.path') || 'python3';

        // CLI script path calculation using extension context (most reliable)
        if (extensionContext) {
            // Use extension context path - this is always correct
            const rulesDslRoot = path.dirname(extensionContext.extensionPath);
            this.cliScriptPath = path.join(rulesDslRoot, 'backend', 'generate_code_cli.py');
            console.log('[CLIClient] Using extensionContext.extensionPath:', extensionContext.extensionPath);
        } else {
            // Fallback to __dirname calculation
            const extensionPath = path.dirname(path.dirname(__dirname)); // Up from out/src to extension root
            const rulesDslRoot = path.dirname(extensionPath); // Up from extension to rules-dsl
            this.cliScriptPath = path.join(rulesDslRoot, 'backend', 'generate_code_cli.py');
            console.log('[CLIClient] Using __dirname fallback:', __dirname);
        }

        // Debug logging
        console.log('[CLIClient] cliScriptPath:', this.cliScriptPath);
        console.log('[CLIClient] pythonPath:', this.pythonPath);
    }

    /**
     * Generate Java code from a .rules file using CLI
     */
    async generateCode(ruleFilePath: string): Promise<CLIResult> {
        return new Promise((resolve, reject) => {
            const process = spawn(this.pythonPath, [this.cliScriptPath, ruleFilePath]);

            let stdout = '';
            let stderr = '';

            process.stdout.on('data', (data) => {
                stdout += data.toString();
            });

            process.stderr.on('data', (data) => {
                stderr += data.toString();
            });

            process.on('close', (code) => {
                if (code === 0 && stdout) {
                    try {
                        const result = JSON.parse(stdout);
                        resolve(result);
                    } catch (error) {
                        reject(new Error(`Failed to parse CLI output: ${error}\nOutput: ${stdout}`));
                    }
                } else {
                    // Try to parse error from stdout if available
                    if (stdout) {
                        try {
                            const result = JSON.parse(stdout);
                            resolve(result); // Even errors are returned as JSON
                        } catch (error) {
                            reject(new Error(`CLI failed with code ${code}\nStderr: ${stderr}\nStdout: ${stdout}`));
                        }
                    } else {
                        reject(new Error(`CLI failed with code ${code}\nStderr: ${stderr}`));
                    }
                }
            });

            process.on('error', (error) => {
                reject(new Error(`Failed to start Python process: ${error.message}\nPython path: ${this.pythonPath}\nCLI script: ${this.cliScriptPath}`));
            });
        });
    }

    /**
     * Validate rule content without generating code using CLI
     */
    async validateOnly(ruleContent: string): Promise<ValidationResult> {
        return new Promise((resolve, reject) => {
            // Create temporary file for validation
            const tempDir = os.tmpdir();
            const tempFile = path.join(tempDir, `rule_validate_${Date.now()}.rules`);

            try {
                // Write rule content to temp file
                fs.writeFileSync(tempFile, ruleContent, 'utf8');

                const process = spawn(this.pythonPath, [this.cliScriptPath, tempFile, '--validate-only']);

                let stdout = '';
                let stderr = '';

                process.stdout.on('data', (data) => {
                    stdout += data.toString();
                });

                process.stderr.on('data', (data) => {
                    stderr += data.toString();
                });

                process.on('close', (code) => {
                    // Clean up temp file
                    try {
                        if (fs.existsSync(tempFile)) {
                            fs.unlinkSync(tempFile);
                        }
                    } catch (cleanupError) {
                        console.warn('Failed to clean up temp file:', cleanupError);
                    }

                    if (stdout) {
                        try {
                            const result = JSON.parse(stdout);
                            resolve(result);
                        } catch (error) {
                            reject(new Error(`Failed to parse validation output: ${error}\nOutput: ${stdout}`));
                        }
                    } else {
                        reject(new Error(`CLI validation failed with code ${code}\nStderr: ${stderr}`));
                    }
                });

                process.on('error', (error) => {
                    // Clean up temp file on error
                    try {
                        if (fs.existsSync(tempFile)) {
                            fs.unlinkSync(tempFile);
                        }
                    } catch (cleanupError) {
                        console.warn('Failed to clean up temp file:', cleanupError);
                    }
                    reject(new Error(`Failed to start validation process: ${error.message}`));
                });
            } catch (error) {
                // Clean up temp file if file creation failed
                try {
                    if (fs.existsSync(tempFile)) {
                        fs.unlinkSync(tempFile);
                    }
                } catch (cleanupError) {
                    console.warn('Failed to clean up temp file:', cleanupError);
                }
                reject(new Error(`Failed to create temp file for validation: ${error}`));
            }
        });
    }
}
