package com.rules.cli;

/**
 * Main CLI entry point for the Rules Engine.
 * Provides unified access to validation and testing functionality.
 */
public class RulesEngineCLI {
    
    public static void main(String[] args) {
        if (args.length == 0) {
            printUsage();
            System.exit(1);
        }
        
        String command = args[0];
        String[] commandArgs = new String[args.length - 1];
        System.arraycopy(args, 1, commandArgs, 0, args.length - 1);
        
        switch (command) {
            case "validate":
                RuleValidator.main(commandArgs);
                break;
            case "test":
                RuleTester.main(commandArgs);
                break;
            case "help":
            case "--help":
            case "-h":
                printUsage();
                break;
            default:
                System.err.println("Unknown command: " + command);
                printUsage();
                System.exit(1);
        }
    }
    
    private static void printUsage() {
        System.out.println("Rules Engine CLI");
        System.out.println("================");
        System.out.println();
        System.out.println("Usage: java -jar rules-engine-java.jar <command> [options]");
        System.out.println();
        System.out.println("Commands:");
        System.out.println("  validate <rule-file>              Validate rule syntax");
        System.out.println("  test <rule-file> <data-file>      Test rule with sample data");
        System.out.println("  help                              Show this help message");
        System.out.println();
        System.out.println("Examples:");
        System.out.println("  java -jar rules-engine-java.jar validate my-rule.rules");
        System.out.println("  java -jar rules-engine-java.jar test my-rule.rules test-data.json");
    }
}