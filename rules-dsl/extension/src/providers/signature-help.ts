import * as vscode from 'vscode';

/**
 * Provides signature help for action functions.
 * Shows parameter information when typing function calls.
 */
export class RulesSignatureHelpProvider implements vscode.SignatureHelpProvider {
    // Known action signatures (can be loaded from workspace config in the future)
    private readonly actionSignatures: Map<string, ActionSignature> = new Map([
        ['approveApplication', {
            label: 'approveApplication()',
            documentation: 'Approves the credit card application',
            parameters: []
        }],
        ['rejectApplication', {
            label: 'rejectApplication(reason: string)',
            documentation: 'Rejects the credit card application with a reason',
            parameters: [
                { label: 'reason', documentation: 'Reason for rejection' }
            ]
        }],
        ['sendEmail', {
            label: 'sendEmail(template: string, recipient: string)',
            documentation: 'Sends an email using the specified template',
            parameters: [
                { label: 'template', documentation: 'Email template name' },
                { label: 'recipient', documentation: 'Email recipient address' }
            ]
        }],
        ['createAccount', {
            label: 'createAccount(accountType: string, initialLimit: number)',
            documentation: 'Creates a new account with specified type and credit limit',
            parameters: [
                { label: 'accountType', documentation: 'Type of account (e.g., "premium", "standard")' },
                { label: 'initialLimit', documentation: 'Initial credit limit amount' }
            ]
        }],
        ['issueCard', {
            label: 'issueCard(cardType: string)',
            documentation: 'Issues a credit card of the specified type',
            parameters: [
                { label: 'cardType', documentation: 'Card type (e.g., "platinum", "gold", "standard")' }
            ]
        }],
        ['calculateLimit', {
            label: 'calculateLimit(score: number, income: number)',
            documentation: 'Calculates credit limit based on credit score and income',
            parameters: [
                { label: 'score', documentation: 'Credit score (300-850)' },
                { label: 'income', documentation: 'Annual income' }
            ]
        }],
        ['checkFraud', {
            label: 'checkFraud()',
            documentation: 'Performs fraud detection check',
            parameters: []
        }]
    ]);

    provideSignatureHelp(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken,
        context: vscode.SignatureHelpContext
    ): vscode.SignatureHelp | undefined {
        const line = document.lineAt(position.line).text;
        const prefix = line.substring(0, position.character);

        // Find the function being typed: actionName(
        const funcMatch = prefix.match(/(\w+)\s*\(/);
        if (!funcMatch) {
            return undefined;
        }

        const actionName = funcMatch[1];
        const signature = this.actionSignatures.get(actionName);

        if (!signature) {
            return undefined;
        }

        // Create signature information
        const sigInfo = new vscode.SignatureInformation(
            signature.label,
            new vscode.MarkdownString(signature.documentation)
        );

        // Add parameters
        for (const param of signature.parameters) {
            const paramInfo = new vscode.ParameterInformation(
                param.label,
                new vscode.MarkdownString(param.documentation)
            );
            sigInfo.parameters.push(paramInfo);
        }

        // Determine which parameter is currently being typed
        const openParenIndex = prefix.lastIndexOf('(');
        const afterParen = prefix.substring(openParenIndex + 1);
        const commaCount = (afterParen.match(/,/g) || []).length;

        const sigHelp = new vscode.SignatureHelp();
        sigHelp.signatures = [sigInfo];
        sigHelp.activeSignature = 0;
        sigHelp.activeParameter = Math.min(commaCount, signature.parameters.length - 1);

        return sigHelp;
    }
}

interface ActionSignature {
    label: string;
    documentation: string;
    parameters: Array<{
        label: string;
        documentation: string;
    }>;
}
