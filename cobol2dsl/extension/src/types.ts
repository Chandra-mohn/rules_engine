/**
 * Type definitions for COBOL-to-DSL Migration Workbench
 */

/**
 * Result from COBOL transpiler conversion
 */
export interface ConversionResult {
    dsl: string;
    metadata: ConversionMetadata;
    warnings?: string[];
    errors?: string[];
}

/**
 * Metadata about the conversion process
 */
export interface ConversionMetadata {
    mappings: AttributeMapping[];
    sourceLines?: number;
    targetLines?: number;
    conversionTime?: number;
}

/**
 * Individual attribute mapping from COBOL to target system
 */
export interface AttributeMapping {
    cobol: string;
    target: string;
    type: 'direct' | 'derived' | 'temporary' | 'action';
    confidence: number;
    comment?: string;
}

/**
 * Transpiler configuration options
 */
export interface TranspilerConfig {
    pythonPath?: string;
    transpilerPath: string;
    ruleName?: string;
    timeout?: number;
}

/**
 * Workbench state for persistence
 */
export interface WorkbenchState {
    cobolContent: string;
    dslContent: string;
    accumulatedRules: string[];
    lastConversion?: Date;
}
