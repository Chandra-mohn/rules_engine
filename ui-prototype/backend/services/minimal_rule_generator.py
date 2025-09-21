"""
Minimal Rule Executor Generation
Generates ONLY rule business logic - infrastructure is hard-coded.
Focuses on 5-10% code generation for maximum performance with minimal complexity.
"""

from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
import re
from datetime import datetime


@dataclass
class RuleDefinition:
    """Represents a single rule for code generation."""
    rule_id: str
    rule_name: str
    rule_content: str
    rule_type: str  # rule, actionset, mon_rule, non_mon_rule
    transaction_code: str
    estimated_steps: int
    is_hot_path: bool  # True if <= 5 steps


@dataclass
class ClientRuleSet:
    """Collection of rules for a specific client."""
    client_id: str
    version: str
    rules: List[RuleDefinition]
    package_name: str = "com.rules.generated"


class MinimalRuleGenerator:
    """
    Generates only rule executor classes - everything else is hard-coded.

    Generated per rule:
    - Individual RuleExecutor implementation with business logic
    - Hot path rules: Fully inlined execution (<=5 steps)
    - Cold path rules: Method-based execution (>5 steps)

    Generated per client:
    - ClientRuleMap with static transaction routing
    - RouterRegistry for client initialization

    NOT generated (hard-coded):
    - UniversalTransactionRouter
    - TransactionContext
    - PerformanceMetrics
    - Caching infrastructure
    - All framework components
    """

    def __init__(self):
        self.hot_path_threshold = 5

    def generate_client_rule_set(self, client_rules: ClientRuleSet) -> Dict[str, str]:
        """
        Generate complete rule set for a client.

        Returns only the minimal generated code:
        - Rule executor classes (business logic only)
        - Client routing map (transaction code → executor mapping)
        - Router registry (client initialization)
        """

        generated_files = {}

        # 1. Generate individual rule executors (core business logic)
        for rule in client_rules.rules:
            executor_files = self._generate_rule_executor(client_rules, rule)
            generated_files.update(executor_files)

        # 2. Generate client routing map (transaction routing only)
        router_map = self._generate_client_router_map(client_rules)
        package_path = client_rules.package_name.replace(".", "/")
        client_id_lower = client_rules.client_id.lower()
        generated_files[f"{package_path}/{client_id_lower}/Client{client_rules.client_id.upper()}RuleMap.java"] = router_map

        # 3. Generate router registry (client initialization only)
        registry = self._generate_router_registry(client_rules)
        generated_files[f"{package_path}/{client_id_lower}/RouterRegistry.java"] = registry

        return generated_files

    def _generate_rule_executor(self, client_rules: ClientRuleSet, rule: RuleDefinition) -> Dict[str, str]:
        """Generate single rule executor with business logic only."""

        package_path = client_rules.package_name.replace(".", "/")
        client_id_lower = client_rules.client_id.lower()
        executor_class = f"{self._to_pascal_case(rule.rule_name)}Executor"

        if rule.is_hot_path:
            executor_code = self._generate_hot_path_executor(client_rules, rule)
        else:
            executor_code = self._generate_cold_path_executor(client_rules, rule)

        return {
            f"{package_path}/{client_id_lower}/executors/{executor_class}.java": executor_code
        }

    def _generate_hot_path_executor(self, client_rules: ClientRuleSet, rule: RuleDefinition) -> str:
        """Generate fully inlined executor for hot path (<=5 steps)."""

        package_name = f"{client_rules.package_name}.{client_rules.client_id.lower()}"
        executor_class = f"{self._to_pascal_case(rule.rule_name)}Executor"

        # Parse rule content to extract business logic
        business_logic = self._parse_rule_to_java(rule.rule_content, is_hot_path=True)

        return f"""package {package_name}.executors;

import com.rules.engine.core.*;

/**
 * HOT PATH EXECUTOR - Fully inlined for maximum performance
 *
 * Rule: {rule.rule_name}
 * Transaction: {rule.transaction_code}
 * Steps: {rule.estimated_steps} (hot path ≤ {self.hot_path_threshold})
 * Generated: {datetime.now().isoformat()}
 */
public final class {executor_class} implements RuleExecutor {{

    @Override
    public RuleResult execute(TransactionContext context) {{
        // Fully inlined business logic for hot path performance
{business_logic}
    }}
}}"""

    def _generate_cold_path_executor(self, client_rules: ClientRuleSet, rule: RuleDefinition) -> str:
        """Generate method-based executor for cold path (>5 steps)."""

        package_name = f"{client_rules.package_name}.{client_rules.client_id.lower()}"
        executor_class = f"{self._to_pascal_case(rule.rule_name)}Executor"

        # Parse rule content to extract business logic with method decomposition
        business_logic = self._parse_rule_to_java(rule.rule_content, is_hot_path=False)

        return f"""package {package_name}.executors;

import com.rules.engine.core.*;

/**
 * COLD PATH EXECUTOR - Method-based execution for complex rules
 *
 * Rule: {rule.rule_name}
 * Transaction: {rule.transaction_code}
 * Steps: {rule.estimated_steps} (cold path > {self.hot_path_threshold})
 * Generated: {datetime.now().isoformat()}
 */
public final class {executor_class} implements RuleExecutor {{

    @Override
    public RuleResult execute(TransactionContext context) {{
        // Complex rule execution with method calls
{business_logic}
    }}

    // Helper methods for complex rule logic (cold path acceptable)
{self._generate_helper_methods(rule.rule_content)}
}}"""

    def _generate_client_router_map(self, client_rules: ClientRuleSet) -> str:
        """Generate client-specific router map with transaction routing."""

        package_name = f"{client_rules.package_name}.{client_rules.client_id.lower()}"
        client_class = f"Client{client_rules.client_id.upper()}RuleMap"

        # Separate hot and cold path rules for optimization
        hot_rules = [r for r in client_rules.rules if r.is_hot_path]
        cold_rules = [r for r in client_rules.rules if not r.is_hot_path]

        # Generate static maps
        hot_executors_map = self._generate_executor_map(hot_rules, "HOT_EXECUTORS")
        cold_executors_map = self._generate_executor_map(cold_rules, "COLD_EXECUTORS")

        # Generate optimized routing logic
        routing_logic = self._generate_optimized_routing_logic(hot_rules, cold_rules)

        return f"""package {package_name};

import com.rules.engine.core.*;
import {package_name}.executors.*;
import java.util.Map;
import java.util.Set;

/**
 * Client-specific rule router for {client_rules.client_id}
 * Optimized for 80K+ TPS with hot/cold path separation
 *
 * Hot path rules: {len(hot_rules)} (≤{self.hot_path_threshold} steps)
 * Cold path rules: {len(cold_rules)} (>{self.hot_path_threshold} steps)
 * Generated: {datetime.now().isoformat()}
 */
public final class {client_class} implements ClientRuleMap {{

    // Hot path executors (80% of traffic, inlined execution)
    private static final Map<String, RuleExecutor> HOT_EXECUTORS = Map.of({hot_executors_map});

    // Cold path executors (20% of traffic, method-based execution)
    private static final Map<String, RuleExecutor> COLD_EXECUTORS = Map.of({cold_executors_map});

    // All supported transaction codes for validation
    private static final Set<String> SUPPORTED_CODES = Set.of({self._generate_supported_codes(client_rules.rules)});

    @Override
    public RuleResult execute(String transactionCode, TransactionContext context) {{
{routing_logic}
    }}

    @Override
    public String getClientId() {{
        return "{client_rules.client_id}";
    }}

    @Override
    public Set<String> getSupportedTransactionCodes() {{
        return SUPPORTED_CODES;
    }}

    @Override
    public void warmUp() {{
        // Warm up hot path executors for JIT optimization
        TransactionContext warmupContext = ContextPool.acquireContext("warmup");

        // Execute each hot path rule once to trigger JIT compilation
{self._generate_warmup_logic(hot_rules)}

        ContextPool.releaseContext(warmupContext);
    }}
}}"""

    def _generate_router_registry(self, client_rules: ClientRuleSet) -> str:
        """Generate router registry for client initialization."""

        package_name = f"{client_rules.package_name}.{client_rules.client_id.lower()}"
        client_class = f"Client{client_rules.client_id.upper()}RuleMap"

        return f"""package {package_name};

import com.rules.engine.core.UniversalTransactionRouter;

/**
 * Router registry for {client_rules.client_id}
 * Initializes client-specific router with the universal system
 */
public final class RouterRegistry {{

    /**
     * Initialize {client_rules.client_id} router
     * Called during application startup
     */
    public static void initialize() {{
        {client_class} clientRouter = new {client_class}();

        // Register with universal router
        UniversalTransactionRouter.registerClient("{client_rules.client_id}", clientRouter);

        // Perform JIT warmup
        clientRouter.warmUp();
    }}
}}"""

    def _parse_rule_to_java(self, rule_content: str, is_hot_path: bool) -> str:
        """
        Parse DSL rule content and convert to Java business logic.

        For hot path: Generate fully inlined code
        For cold path: Generate method-based code
        """

        # Extract rule structure using simple parsing
        rule_structure = self._extract_rule_structure(rule_content)

        if is_hot_path:
            return self._generate_inlined_java(rule_structure)
        else:
            return self._generate_method_based_java(rule_structure)

    def _extract_rule_structure(self, rule_content: str) -> Dict[str, Any]:
        """Extract rule structure from DSL content."""

        # Simple pattern matching for common rule patterns
        structure = {
            "conditions": [],
            "actions": [],
            "rule_type": "simple"
        }

        # Extract IF-THEN patterns
        if_then_pattern = r'if\s+(.+?)\s+then\s+(.+?)(?:\s+else\s+(.+?))?(?=\s*(?:if|$))'
        matches = re.findall(if_then_pattern, rule_content, re.IGNORECASE | re.DOTALL)

        for match in matches:
            condition = match[0].strip()
            then_action = match[1].strip()
            else_action = match[2].strip() if match[2] else None

            structure["conditions"].append({
                "condition": condition,
                "then_action": then_action,
                "else_action": else_action
            })

        # Extract standalone actions
        action_pattern = r'^\\s*([a-zA-Z_][a-zA-Z0-9_]*(?:\\([^)]*\\))?)\\s*$'
        lines = rule_content.split('\\n')
        for line in lines:
            line = line.strip()
            if line and not any(keyword in line.lower() for keyword in ['if', 'then', 'else', 'rule']):
                if re.match(action_pattern, line):
                    structure["actions"].append(line)

        return structure

    def _generate_inlined_java(self, rule_structure: Dict[str, Any]) -> str:
        """Generate fully inlined Java code for hot path."""

        java_lines = []
        java_lines.append("        // Hot path: fully inlined execution")

        # Generate condition checks
        for i, cond_block in enumerate(rule_structure["conditions"]):
            condition_java = self._convert_condition_to_java(cond_block["condition"])
            then_action_java = self._convert_action_to_java(cond_block["then_action"])

            if i == 0:
                java_lines.append(f"        if ({condition_java}) {{")
            else:
                java_lines.append(f"        }} else if ({condition_java}) {{")

            java_lines.append(f"            // Execute: {cond_block['then_action']}")
            java_lines.extend([f"            {line}" for line in then_action_java.split('\\n')])

            if cond_block["else_action"]:
                else_action_java = self._convert_action_to_java(cond_block["else_action"])
                java_lines.append(f"        }} else {{")
                java_lines.append(f"            // Execute: {cond_block['else_action']}")
                java_lines.extend([f"            {line}" for line in else_action_java.split('\\n')])

        # Close final condition
        if rule_structure["conditions"]:
            java_lines.append("        }")

        # Generate standalone actions
        for action in rule_structure["actions"]:
            action_java = self._convert_action_to_java(action)
            java_lines.append(f"        // Execute: {action}")
            java_lines.extend([f"        {line}" for line in action_java.split('\\n')])

        # Default return
        java_lines.append("        ")
        java_lines.append("        return RuleResult.success(context);")

        return "\\n".join(java_lines)

    def _generate_method_based_java(self, rule_structure: Dict[str, Any]) -> str:
        """Generate method-based Java code for cold path."""

        java_lines = []
        java_lines.append("        // Cold path: method-based execution")
        java_lines.append("        return executeRuleLogic(context);")

        return "\\n".join(java_lines)

    def _generate_helper_methods(self, rule_content: str) -> str:
        """Generate helper methods for cold path execution."""

        return f"""
    private RuleResult executeRuleLogic(TransactionContext context) {{
        // Complex rule logic broken into methods
        TransactionContext step1Result = executeStep1(context);
        TransactionContext step2Result = executeStep2(step1Result);
        return RuleResult.success(step2Result);
    }}

    private TransactionContext executeStep1(TransactionContext context) {{
        // TODO: Implement actual step 1 logic from: {rule_content[:50]}...
        return context;
    }}

    private TransactionContext executeStep2(TransactionContext context) {{
        // TODO: Implement actual step 2 logic
        return context;
    }}"""

    def _convert_condition_to_java(self, condition: str) -> str:
        """Convert DSL condition to Java expression."""

        # Simple conversions for common patterns
        java_condition = condition

        # Convert attribute access
        java_condition = re.sub(r'(\\w+)\\.(\\w+)', r'context.get\\2()', java_condition)

        # Convert operators
        java_condition = java_condition.replace(' and ', ' && ')
        java_condition = java_condition.replace(' or ', ' || ')
        java_condition = java_condition.replace(' not ', ' ! ')

        return java_condition

    def _convert_action_to_java(self, action: str) -> str:
        """Convert DSL action to Java statements."""

        # Simple action conversions
        if "approve" in action.lower():
            return f"""TransactionContext approved = context.withStatus("APPROVED");
return RuleResult.success(approved);"""

        elif "reject" in action.lower():
            return f"""TransactionContext rejected = context.withStatus("REJECTED");
return RuleResult.rejected(rejected, "Rule rejected transaction");"""

        else:
            # Generic action execution
            return f"""// Execute action: {action}
return RuleResult.success(context);"""

    def _generate_executor_map(self, rules: List[RuleDefinition], map_name: str) -> str:
        """Generate static map of transaction codes to executors."""

        if not rules:
            return ""

        entries = []
        for rule in rules:
            executor_class = f"{self._to_pascal_case(rule.rule_name)}Executor"
            entries.append(f'""{rule.transaction_code}"", new {executor_class}()')

        return ",\\n            ".join(entries)

    def _generate_supported_codes(self, rules: List[RuleDefinition]) -> str:
        """Generate set of supported transaction codes."""

        codes = [f'"{rule.transaction_code}"' for rule in rules]
        return ", ".join(codes)

    def _generate_optimized_routing_logic(self, hot_rules: List[RuleDefinition],
                                        cold_rules: List[RuleDefinition]) -> str:
        """Generate branch-prediction optimized routing logic."""

        # Most frequent transaction codes first for CPU branch prediction
        routing_lines = []
        routing_lines.append("        // Branch prediction optimized routing")

        # Fast path for most frequent hot transactions
        if hot_rules:
            most_frequent = hot_rules[:3]  # Assume first 3 are most frequent
            for rule in most_frequent:
                routing_lines.append(f'        if ("{rule.transaction_code}".equals(transactionCode)) {{')
                routing_lines.append(f'            return HOT_EXECUTORS.get(transactionCode).execute(context);')
                routing_lines.append('        }')

        # General hot path check
        routing_lines.append("")
        routing_lines.append("        // Check hot path executors first (80% of traffic)")
        routing_lines.append("        RuleExecutor hotExecutor = HOT_EXECUTORS.get(transactionCode);")
        routing_lines.append("        if (hotExecutor != null) {")
        routing_lines.append("            return hotExecutor.execute(context);")
        routing_lines.append("        }")
        routing_lines.append("")

        # Cold path fallback
        routing_lines.append("        // Fall back to cold path executors (20% of traffic)")
        routing_lines.append("        RuleExecutor coldExecutor = COLD_EXECUTORS.get(transactionCode);")
        routing_lines.append("        if (coldExecutor != null) {")
        routing_lines.append("            return coldExecutor.execute(context);")
        routing_lines.append("        }")
        routing_lines.append("")

        # Unknown transaction
        routing_lines.append("        // Unknown transaction code")
        routing_lines.append("        return RuleResult.unknownTransaction(transactionCode);")

        return "\\n".join(routing_lines)

    def _generate_warmup_logic(self, hot_rules: List[RuleDefinition]) -> str:
        """Generate JIT warmup logic for hot path rules."""

        warmup_lines = []
        for rule in hot_rules:
            warmup_lines.append(f'        HOT_EXECUTORS.get("{rule.transaction_code}").execute(warmupContext);')

        return "\\n".join(warmup_lines)

    def _to_pascal_case(self, text: str) -> str:
        """Convert text to PascalCase."""
        words = text.replace('-', ' ').replace('_', ' ').split()
        return ''.join(word.capitalize() for word in words)


class RuleSetBuilder:
    """Helper to build rule sets from database/configuration."""

    @staticmethod
    def from_database_rules(client_id: str, version: str, db_rules: List[Dict[str, Any]]) -> ClientRuleSet:
        """Build ClientRuleSet from database rule records."""

        rules = []
        for db_rule in db_rules:
            rule_def = RuleDefinition(
                rule_id=str(db_rule.get('id', '')),
                rule_name=db_rule.get('name', ''),
                rule_content=db_rule.get('content', ''),
                rule_type=db_rule.get('item_type', 'rule'),
                transaction_code=db_rule.get('transaction_code', db_rule.get('name', '')),
                estimated_steps=RuleSetBuilder._estimate_rule_steps(db_rule.get('content', '')),
                is_hot_path=False  # Will be set by estimated_steps
            )

            # Determine if hot path based on estimated steps
            rule_def.is_hot_path = rule_def.estimated_steps <= 5

            rules.append(rule_def)

        return ClientRuleSet(
            client_id=client_id,
            version=version,
            rules=rules
        )

    @staticmethod
    def _estimate_rule_steps(rule_content: str) -> int:
        """Estimate number of execution steps in a rule."""

        # Simple heuristic based on rule complexity
        lines = [line.strip() for line in rule_content.split('\\n') if line.strip()]

        # Count significant lines (conditions, actions)
        significant_lines = 0
        for line in lines:
            if any(keyword in line.lower() for keyword in ['if', 'then', 'else', 'action']):
                significant_lines += 1

        # Minimum 1 step, typical ranges
        return max(1, significant_lines)


# Convenience functions for integration
def generate_minimal_rules(client_id: str, version: str, db_rules: List[Dict[str, Any]]) -> Dict[str, str]:
    """Generate minimal rule code from database rules."""

    # Build rule set from database
    client_rules = RuleSetBuilder.from_database_rules(client_id, version, db_rules)

    # Generate minimal code
    generator = MinimalRuleGenerator()
    return generator.generate_client_rule_set(client_rules)