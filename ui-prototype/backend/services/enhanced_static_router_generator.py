"""
Enhanced Static Router Generator with DSL Integration
Replaces TODO comments with actual rule logic generation for 80K+ TPS performance.
"""

from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from pathlib import Path
import json
import hashlib
from datetime import datetime

# Import the base static router generator
from .static_router_generator import StaticRouterGenerator, TransactionMapping, ClientRouterSpec

# Import our advanced DSL generator
from ..grammar_parser.advanced_java_generator import (
    AdvancedJavaCodeGenerator,
    PerformanceAnalyzer,
    RuleAnalysis,
    OptimizedJavaCode
)


@dataclass
class EnhancedTransactionMapping(TransactionMapping):
    """Enhanced transaction mapping with DSL rule content."""
    rule_content: Optional[str] = None
    rule_analysis: Optional[RuleAnalysis] = None


class EnhancedStaticRouterGenerator(StaticRouterGenerator):
    """
    Enhanced static router generator with DSL integration.
    Replaces TODO comments with actual optimized rule logic.
    """

    def __init__(self):
        super().__init__()
        self.java_generator = AdvancedJavaCodeGenerator()
        self.performance_analyzer = PerformanceAnalyzer()

    def generate_universal_router_with_rules(self,
                                           client_specs: List[ClientRouterSpec],
                                           rules_database: Optional[Dict[str, str]] = None) -> Dict[str, str]:
        """
        Generate universal router with actual rule content from database.

        Args:
            client_specs: List of client router specifications
            rules_database: Optional mapping of rule_name -> rule_content

        Returns:
            Dict mapping file paths to Java source code
        """
        # Enhance client specs with rule content
        enhanced_client_specs = self._enhance_client_specs_with_rules(client_specs, rules_database)

        # Generate router using enhanced specs
        return self.generate_universal_router(enhanced_client_specs)

    def _enhance_client_specs_with_rules(self,
                                       client_specs: List[ClientRouterSpec],
                                       rules_database: Optional[Dict[str, str]]) -> List[ClientRouterSpec]:
        """Enhance client specifications with actual rule content."""
        enhanced_specs = []

        for spec in client_specs:
            enhanced_mappings = []

            for mapping in spec.transaction_mappings:
                # Get rule content from database if available
                rule_content = None
                if rules_database and mapping.rule_name in rules_database:
                    rule_content = rules_database[mapping.rule_name]

                # Create enhanced mapping
                enhanced_mapping = EnhancedTransactionMapping(
                    transaction_code=mapping.transaction_code,
                    rule_id=mapping.rule_id,
                    rule_name=mapping.rule_name,
                    rule_type=mapping.rule_type,
                    execution_frequency=mapping.execution_frequency,
                    dependencies=mapping.dependencies,
                    estimated_steps=mapping.estimated_steps,
                    context_size_kb=mapping.context_size_kb,
                    rule_content=rule_content
                )

                # Analyze rule if content is available
                if rule_content:
                    enhanced_mapping.rule_analysis = self.performance_analyzer.analyze_rule(rule_content)
                    # Update estimated steps based on analysis
                    enhanced_mapping.estimated_steps = enhanced_mapping.rule_analysis.estimated_steps

                enhanced_mappings.append(enhanced_mapping)

            # Create enhanced client spec
            enhanced_spec = ClientRouterSpec(
                client_id=spec.client_id,
                transaction_mappings=enhanced_mappings,
                hot_path_threshold=spec.hot_path_threshold,
                package_name=spec.package_name
            )
            enhanced_specs.append(enhanced_spec)

        return enhanced_specs

    def _generate_hot_path_executor(self, mapping: EnhancedTransactionMapping, package: str) -> str:
        """Generate optimized hot path executor with actual DSL logic."""
        executor_class = f"{self._to_pascal_case(mapping.rule_name)}Executor"

        # Generate actual rule logic from DSL
        if mapping.rule_content:
            optimized_code = self.java_generator.generate_optimized_executor_code(
                mapping.rule_content, mapping.rule_name, 'hot'
            )
            rule_logic = optimized_code.java_code
            optimization_comments = f"// Optimizations applied: {', '.join(optimized_code.optimization_applied)}"
        else:
            # Fallback to high-performance template
            rule_logic = self._generate_fallback_hot_path_logic()
            optimization_comments = "// Using fallback hot path logic - no DSL content available"

        return f"""package {package}.executors;

import com.rules.router.*;

/**
 * Hot path executor for {mapping.rule_name}
 * Fully inlined for maximum performance
 * Estimated steps: {mapping.estimated_steps}
 * Transaction code: {mapping.transaction_code}
 * {optimization_comments}
 * Generated at {datetime.now().isoformat()}
 */
public class {executor_class} implements RuleExecutor {{

    @Override
    public RuleResult execute(TransactionContext context) {{
{rule_logic}
    }}

    // Inlined helper methods for maximum performance
    private static int calculateLimit(TransactionContext context) {{
        return Math.min((int)(context.getIncome() * 3), 50000);
    }}

    private static double getStandardAPR(TransactionContext context) {{
        return context.getCreditScore() >= 750 ? 12.99 : 15.99;
    }}

    private static boolean isHighRiskMerchant(TransactionContext context) {{
        Object merchant = context.getExtended("merchantCategory");
        return "HIGH_RISK".equals(merchant) || "GAMBLING".equals(merchant);
    }}

    private static boolean hasValidEmployment(TransactionContext context) {{
        Object employment = context.getExtended("employmentStatus");
        return "employed".equals(employment) || "self-employed".equals(employment);
    }}
}}"""

    def _generate_cold_path_executor(self, mapping: EnhancedTransactionMapping, package: str) -> str:
        """Generate optimized cold path executor with actual DSL logic."""
        executor_class = f"{self._to_pascal_case(mapping.rule_name)}Executor"

        # Generate actual rule logic from DSL
        if mapping.rule_content:
            optimized_code = self.java_generator.generate_optimized_executor_code(
                mapping.rule_content, mapping.rule_name, 'cold'
            )
            rule_logic = optimized_code.java_code
            optimization_comments = f"// Optimizations applied: {', '.join(optimized_code.optimization_applied)}"
        else:
            # Fallback to structured template
            rule_logic = self._generate_fallback_cold_path_logic()
            optimization_comments = "// Using fallback cold path logic - no DSL content available"

        return f"""package {package}.executors;

import com.rules.router.*;
import java.util.Map;
import java.util.HashMap;

/**
 * Cold path executor for {mapping.rule_name}
 * Method-based execution for complex rules
 * Estimated steps: {mapping.estimated_steps}
 * Transaction code: {mapping.transaction_code}
 * {optimization_comments}
 * Generated at {datetime.now().isoformat()}
 */
public class {executor_class} implements RuleExecutor {{

    @Override
    public RuleResult execute(TransactionContext context) {{
{rule_logic}
    }}
}}"""

    def _generate_fallback_hot_path_logic(self) -> str:
        """Generate high-performance fallback hot path logic."""
        return """        // Hot path execution - fully inlined for maximum performance
        long startNanos = System.nanoTime();
        boolean ruleMatched = false;

        // Fast path: Common credit score validation
        int creditScore = context.getCreditScore();
        double income = context.getIncome();

        // Optimized decision tree for common scenarios
        if (creditScore >= 750 && income >= 75000) {
            // Premium approval path (fastest)
            TransactionContext approved = context
                .withStatus("APPROVED")
                .withCreditLimit(calculateLimit(context))
                .withAPR(getStandardAPR(context))
                .withExtended("approvalType", "PREMIUM");
            return RuleResult.success(approved);
        }

        if (creditScore >= 700 && income >= 50000 && hasValidEmployment(context)) {
            // Standard approval path
            TransactionContext approved = context
                .withStatus("APPROVED")
                .withCreditLimit(calculateLimit(context))
                .withAPR(getStandardAPR(context))
                .withExtended("approvalType", "STANDARD");
            return RuleResult.success(approved);
        }

        if (creditScore < 600 || income < 25000 || isHighRiskMerchant(context)) {
            // Fast rejection path
            TransactionContext rejected = context
                .withStatus("REJECTED")
                .withReason("Does not meet minimum criteria");
            return RuleResult.rejected(rejected);
        }

        // Conditional approval for edge cases
        TransactionContext conditional = context
            .withStatus("CONDITIONAL")
            .withReason("Manual review required");
        return RuleResult.success(conditional);"""

    def _generate_fallback_cold_path_logic(self) -> str:
        """Generate structured fallback cold path logic."""
        return """        // Complex rule execution with method decomposition
        return executeComplexRuleChain(context);
    }

    private RuleResult executeComplexRuleChain(TransactionContext context) {
        // Multi-step execution for complex rules
        try {
            // Step 1: Risk assessment
            RiskAssessmentResult riskResult = performRiskAssessment(context);
            if (riskResult.isRejected()) {
                return RuleResult.rejected(context.withReason(riskResult.getReason()));
            }

            // Step 2: Financial verification
            FinancialResult financialResult = performFinancialVerification(context, riskResult);
            if (financialResult.isRejected()) {
                return RuleResult.rejected(context.withReason(financialResult.getReason()));
            }

            // Step 3: Final approval determination
            ApprovalResult approvalResult = makeFinalApprovalDecision(context, riskResult, financialResult);

            if (approvalResult.isApproved()) {
                TransactionContext approved = context
                    .withStatus("APPROVED")
                    .withCreditLimit(approvalResult.getCreditLimit())
                    .withAPR(approvalResult.getAPR())
                    .withExtended("riskLevel", riskResult.getRiskLevel())
                    .withExtended("approvalType", approvalResult.getApprovalType());
                return RuleResult.success(approved);
            } else {
                TransactionContext rejected = context
                    .withStatus("REJECTED")
                    .withReason(approvalResult.getReason());
                return RuleResult.rejected(rejected);
            }

        } catch (Exception e) {
            TransactionContext error = context
                .withStatus("ERROR")
                .withReason("Rule execution failed: " + e.getMessage());
            return RuleResult.error("EXECUTION_ERROR", e.getMessage());
        }
    }

    private RiskAssessmentResult performRiskAssessment(TransactionContext context) {
        // Complex risk assessment logic
        int creditScore = context.getCreditScore();
        double income = context.getIncome();
        Object employment = context.getExtended("employmentStatus");
        Object bankruptcy = context.getExtended("bankruptcyHistory");

        // High risk indicators
        if (Boolean.TRUE.equals(bankruptcy)) {
            return new RiskAssessmentResult(true, "HIGH", "Bankruptcy history");
        }

        if (creditScore < 550) {
            return new RiskAssessmentResult(true, "HIGH", "Credit score too low");
        }

        if ("unemployed".equals(employment) && income < 30000) {
            return new RiskAssessmentResult(true, "HIGH", "Insufficient income and employment");
        }

        // Medium risk
        if (creditScore < 650 || income < 40000) {
            return new RiskAssessmentResult(false, "MEDIUM", null);
        }

        // Low risk
        return new RiskAssessmentResult(false, "LOW", null);
    }

    private FinancialResult performFinancialVerification(TransactionContext context, RiskAssessmentResult riskResult) {
        // Financial verification logic based on risk level
        double income = context.getIncome();
        int creditScore = context.getCreditScore();

        if ("HIGH".equals(riskResult.getRiskLevel())) {
            return new FinancialResult(true, "High risk applicant");
        }

        // Income verification
        if (income < 25000) {
            return new FinancialResult(true, "Insufficient income");
        }

        // Debt-to-income check (simulated)
        Object monthlyDebt = context.getExtended("monthlyDebt");
        if (monthlyDebt instanceof Number) {
            double monthlyIncome = income / 12;
            double debtRatio = ((Number) monthlyDebt).doubleValue() / monthlyIncome;
            if (debtRatio > 0.4) {
                return new FinancialResult(true, "Debt-to-income ratio too high");
            }
        }

        return new FinancialResult(false, null);
    }

    private ApprovalResult makeFinalApprovalDecision(TransactionContext context,
                                                   RiskAssessmentResult riskResult,
                                                   FinancialResult financialResult) {
        int creditScore = context.getCreditScore();
        double income = context.getIncome();
        String riskLevel = riskResult.getRiskLevel();

        // Premium approval criteria
        if (creditScore >= 750 && income >= 100000 && "LOW".equals(riskLevel)) {
            return new ApprovalResult(true, calculatePremiumLimit(income), 12.99, "PREMIUM", null);
        }

        // Standard approval criteria
        if (creditScore >= 700 && income >= 50000 && !"HIGH".equals(riskLevel)) {
            return new ApprovalResult(true, calculateStandardLimit(income), 15.99, "STANDARD", null);
        }

        // Conditional approval
        if (creditScore >= 650 && income >= 35000) {
            return new ApprovalResult(true, calculateConditionalLimit(income), 18.99, "CONDITIONAL", null);
        }

        // Rejection
        return new ApprovalResult(false, 0, 0, null, "Does not meet approval criteria");
    }

    // Helper classes for structured data
    private static class RiskAssessmentResult {
        private final boolean rejected;
        private final String riskLevel;
        private final String reason;

        public RiskAssessmentResult(boolean rejected, String riskLevel, String reason) {
            this.rejected = rejected;
            this.riskLevel = riskLevel;
            this.reason = reason;
        }

        public boolean isRejected() { return rejected; }
        public String getRiskLevel() { return riskLevel; }
        public String getReason() { return reason; }
    }

    private static class FinancialResult {
        private final boolean rejected;
        private final String reason;

        public FinancialResult(boolean rejected, String reason) {
            this.rejected = rejected;
            this.reason = reason;
        }

        public boolean isRejected() { return rejected; }
        public String getReason() { return reason; }
    }

    private static class ApprovalResult {
        private final boolean approved;
        private final int creditLimit;
        private final double apr;
        private final String approvalType;
        private final String reason;

        public ApprovalResult(boolean approved, int creditLimit, double apr, String approvalType, String reason) {
            this.approved = approved;
            this.creditLimit = creditLimit;
            this.apr = apr;
            this.approvalType = approvalType;
            this.reason = reason;
        }

        public boolean isApproved() { return approved; }
        public int getCreditLimit() { return creditLimit; }
        public double getAPR() { return apr; }
        public String getApprovalType() { return approvalType; }
        public String getReason() { return reason; }
    }

    // Helper methods for credit limit calculation
    private static int calculatePremiumLimit(double income) {
        return Math.min((int)(income * 4), 100000);
    }

    private static int calculateStandardLimit(double income) {
        return Math.min((int)(income * 3), 50000);
    }

    private static int calculateConditionalLimit(double income) {
        return Math.min((int)(income * 2), 25000);"""

    def generate_rule_database_loader(self) -> str:
        """Generate utility class for loading rules from database."""
        return """package com.rules.router.util;

import java.util.Map;
import java.util.HashMap;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Utility for loading rule content from database for router generation.
 * Used during build-time to generate optimized executors.
 */
public class RuleDatabaseLoader {

    /**
     * Load all rule contents indexed by rule name.
     */
    public static Map<String, String> loadRuleContents(Connection connection) throws SQLException {
        Map<String, String> ruleContents = new HashMap<>();

        String sql = "SELECT name, content FROM rules WHERE status IN ('VALID', 'PROD') AND content IS NOT NULL";

        try (PreparedStatement stmt = connection.prepareStatement(sql);
             ResultSet rs = stmt.executeQuery()) {

            while (rs.next()) {
                String ruleName = rs.getString("name");
                String content = rs.getString("content");
                ruleContents.put(ruleName, content);
            }
        }

        return ruleContents;
    }

    /**
     * Load rule content for specific transaction mappings.
     */
    public static Map<String, String> loadRuleContentsForMappings(
            Connection connection,
            Map<String, String> transactionToRuleMap) throws SQLException {

        Map<String, String> ruleContents = new HashMap<>();

        String sql = "SELECT name, content FROM rules WHERE name = ? AND status IN ('VALID', 'PROD')";

        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            for (String ruleName : transactionToRuleMap.values()) {
                stmt.setString(1, ruleName);
                try (ResultSet rs = stmt.executeQuery()) {
                    if (rs.next()) {
                        String content = rs.getString("content");
                        ruleContents.put(ruleName, content);
                    }
                }
            }
        }

        return ruleContents;
    }
}"""

    def generate_build_time_router_generator(self) -> str:
        """Generate build-time utility for router generation with database integration."""
        return """package com.rules.router.build;

import com.rules.router.util.RuleDatabaseLoader;
import java.util.*;
import java.sql.Connection;
import java.sql.DriverManager;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Build-time router generator that reads rules from database
 * and generates optimized Java classes.
 */
public class BuildTimeRouterGenerator {

    private final String databaseUrl;
    private final String outputDirectory;

    public BuildTimeRouterGenerator(String databaseUrl, String outputDirectory) {
        this.databaseUrl = databaseUrl;
        this.outputDirectory = outputDirectory;
    }

    /**
     * Generate all router classes based on current database state.
     */
    public void generateRouters() throws Exception {
        // Load rule contents from database
        Map<String, String> ruleContents;
        try (Connection connection = DriverManager.getConnection(databaseUrl)) {
            ruleContents = RuleDatabaseLoader.loadRuleContents(connection);
        }

        // Generate transaction mappings (this would come from configuration)
        List<TransactionMapping> mappings = generateTransactionMappings();

        // Generate client specifications
        List<ClientRouterSpec> clientSpecs = generateClientSpecs(mappings);

        // Create enhanced generator and generate code
        EnhancedStaticRouterGenerator generator = new EnhancedStaticRouterGenerator();
        Map<String, String> generatedFiles = generator.generate_universal_router_with_rules(
            clientSpecs, ruleContents);

        // Write generated files to output directory
        writeGeneratedFiles(generatedFiles);
    }

    private List<TransactionMapping> generateTransactionMappings() {
        // This would typically load from configuration or database
        // For now, return sample mappings
        return Arrays.asList(
            new TransactionMapping("CC_APP_001", "creditScoreCheck", "creditScoreCheck",
                                 "rule", "hot", Arrays.asList(), 3, 2),
            new TransactionMapping("CC_APP_002", "ageVerification", "ageVerification",
                                 "rule", "hot", Arrays.asList(), 2, 1),
            new TransactionMapping("CC_PREMIUM_001", "premiumEligibilityCheck", "premiumEligibilityCheck",
                                 "rule", "cold", Arrays.asList(), 8, 5)
        );
    }

    private List<ClientRouterSpec> generateClientSpecs(List<TransactionMapping> mappings) {
        return Arrays.asList(
            new ClientRouterSpec("DEMO", mappings, 5, "com.rules.generated")
        );
    }

    private void writeGeneratedFiles(Map<String, String> generatedFiles) throws IOException {
        for (Map.Entry<String, String> entry : generatedFiles.entrySet()) {
            String filePath = entry.getKey();
            String content = entry.getValue();

            Path outputPath = Paths.get(outputDirectory, filePath);
            outputPath.getParent().toFile().mkdirs();

            try (FileWriter writer = new FileWriter(outputPath.toFile())) {
                writer.write(content);
            }

            System.out.println("Generated: " + outputPath);
        }
    }

    public static void main(String[] args) throws Exception {
        if (args.length != 2) {
            System.err.println("Usage: BuildTimeRouterGenerator <database-url> <output-directory>");
            System.exit(1);
        }

        String databaseUrl = args[0];
        String outputDirectory = args[1];

        BuildTimeRouterGenerator generator = new BuildTimeRouterGenerator(databaseUrl, outputDirectory);
        generator.generateRouters();

        System.out.println("Router generation completed successfully.");
    }
}"""


def create_enhanced_router_with_rule_integration():
    """Factory function to create the enhanced router generator."""
    return EnhancedStaticRouterGenerator()


# Export the main classes
__all__ = [
    'EnhancedStaticRouterGenerator',
    'EnhancedTransactionMapping',
    'create_enhanced_router_with_rule_integration'
]