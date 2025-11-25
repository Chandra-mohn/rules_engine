/**
 * Bridge to Python COBOL transpiler backend
 * Handles subprocess management and communication
 */

import { spawn } from 'child_process';
import * as path from 'path';
import { ConversionResult, TranspilerConfig } from './types';

export class TranspilerBridge {
    private config: TranspilerConfig;

    constructor(config: TranspilerConfig) {
        this.config = config;
    }

    /**
     * Convert COBOL code to Rules DSL
     * @param cobolCode COBOL source code to convert
     * @param ruleName Optional name for the generated rule
     * @returns Conversion result with DSL and metadata
     */
    async convertToDSL(cobolCode: string, ruleName?: string): Promise<ConversionResult> {
        return new Promise((resolve, reject) => {
            const pythonPath = this.config.pythonPath || 'python3';
            const transpilerScript = path.join(this.config.transpilerPath, 'transpiler', 'cobol_converter.py');

            // Spawn Python process
            const pythonProcess = spawn(pythonPath, [
                transpilerScript,
                '--stdin',
                '--rule-name', ruleName || 'Converted Rule'
            ], {
                cwd: this.config.transpilerPath
            });

            let stdout = '';
            let stderr = '';

            // Collect stdout
            pythonProcess.stdout.on('data', (data) => {
                stdout += data.toString();
            });

            // Collect stderr
            pythonProcess.stderr.on('data', (data) => {
                stderr += data.toString();
            });

            // Handle process completion
            pythonProcess.on('close', (code) => {
                if (code === 0) {
                    try {
                        // Parse JSON output from transpiler
                        const result = JSON.parse(stdout);
                        resolve({
                            dsl: result.dsl || result.code || '',
                            metadata: {
                                mappings: result.metadata?.mappings || result.mappings || [],
                                conversionTime: result.conversionTime
                            },
                            warnings: result.metadata?.warnings || result.warnings,
                            errors: result.metadata?.errors || result.errors
                        });
                    } catch (parseError) {
                        // Fallback: treat stdout as raw DSL if JSON parsing fails
                        resolve({
                            dsl: stdout,
                            metadata: {
                                mappings: []
                            },
                            warnings: [`JSON parse failed: ${parseError}`]
                        });
                    }
                } else {
                    reject(new Error(`Transpiler failed with code ${code}: ${stderr}`));
                }
            });

            // Handle process errors
            pythonProcess.on('error', (error) => {
                reject(new Error(`Failed to spawn transpiler: ${error.message}`));
            });

            // Send COBOL code to stdin
            pythonProcess.stdin.write(cobolCode);
            pythonProcess.stdin.end();

            // Set timeout
            const timeout = this.config.timeout || 30000;
            setTimeout(() => {
                pythonProcess.kill();
                reject(new Error(`Transpiler timeout after ${timeout}ms`));
            }, timeout);
        });
    }

    /**
     * Validate that Python and transpiler are available
     * @returns true if transpiler can be executed
     */
    async validate(): Promise<boolean> {
        try {
            const pythonPath = this.config.pythonPath || 'python3';
            const checkProcess = spawn(pythonPath, ['--version']);

            return new Promise((resolve) => {
                checkProcess.on('close', (code) => {
                    resolve(code === 0);
                });
                checkProcess.on('error', () => {
                    resolve(false);
                });
            });
        } catch {
            return false;
        }
    }
}
