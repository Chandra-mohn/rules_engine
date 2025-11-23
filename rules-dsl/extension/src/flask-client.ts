import axios, { AxiosInstance } from 'axios';

export interface ValidationResult {
    valid: boolean;
    error?: string;
    errors?: Array<{
        line: number;
        column: number;
        message: string;
    }>;
}

export interface SemanticError {
    line: number;
    column: number;
    severity: 'error' | 'warning';
    message: string;
    suggestion?: string;
    type: string;
}

export interface SemanticValidationResult {
    valid: boolean;
    errors: SemanticError[];
    warnings: SemanticError[];
}

export interface CodeGenerationResult {
    success: boolean;
    javaCode?: string;
    error?: string;
}

export class FlaskClient {
    private client: AxiosInstance;

    constructor(baseURL: string) {
        this.client = axios.create({
            baseURL,
            timeout: 10000,
            headers: {
                'Content-Type': 'application/json'
            }
        });
    }

    async validate(content: string): Promise<ValidationResult> {
        try {
            const response = await this.client.post('/api/validate', {
                content
            });
            return response.data;
        } catch (error) {
            if (axios.isAxiosError(error) && error.response) {
                return {
                    valid: false,
                    error: error.response.data.error || 'Validation failed'
                };
            }
            throw error;
        }
    }

    async generateCode(content: string): Promise<CodeGenerationResult> {
        try {
            const response = await this.client.post('/api/generate', {
                content
            });
            return response.data;
        } catch (error) {
            if (axios.isAxiosError(error) && error.response) {
                return {
                    success: false,
                    error: error.response.data.error || 'Generation failed'
                };
            }
            throw error;
        }
    }

    async validateSemantic(content: string): Promise<SemanticValidationResult> {
        try {
            const response = await this.client.post('/api/validate/semantic', {
                content
            });
            return {
                valid: response.data.valid || false,
                errors: response.data.errors || [],
                warnings: response.data.warnings || []
            };
        } catch (error) {
            if (axios.isAxiosError(error) && error.response) {
                return {
                    valid: false,
                    errors: [{
                        line: 1,
                        column: 1,
                        severity: 'error',
                        message: error.response.data.error || 'Semantic validation failed',
                        type: 'validation_error'
                    }],
                    warnings: []
                };
            }
            throw error;
        }
    }

    async testConnection(): Promise<boolean> {
        try {
            await this.client.get('/health');
            return true;
        } catch (error) {
            return false;
        }
    }
}
