"""
Rule Cycle Detector Service

Detects cyclic dependencies in rule calls using static analysis and graph algorithms.
Implements Option C: Hybrid approach with save-time validation + runtime guards.
"""

import re
import logging
from typing import List, Set, Optional, Dict

logger = logging.getLogger(__name__)


class RuleCycleDetector:
    """Detects cyclic dependencies in rule calls."""

    # Known primitive actions (not rules/actionsets)
    PRIMITIVE_ACTIONS = {
        'approveTransaction', 'declineTransaction', 'updateAccountBalance',
        'calculateRiskScore', 'logHighRiskApplicant', 'requireCoSigner',
        'scheduleManualReview', 'approveApplication', 'conditionalApproval',
        'rejectApplication', 'logTransaction', 'sendNotification',
        'requireManualReview', 'flagForReview', 'updateRiskScore',
        'instantApproval', 'immediateReject', 'done', 'validateApplicantInfo',
        'sendWelcomeEmail', 'sendRejectionLetter', 'requestDocumentation',
        'updateCustomerRecord', 'fastTrackApproval', 'assignPrivateBanker',
        'premiumApproval', 'schedulePersonalConsultation'
    }

    # Grammar keywords to ignore
    KEYWORDS = {
        'rule', 'actionset', 'action', 'if', 'then', 'else', 'elseif',
        'endif', 'and', 'or', 'not', 'in', 'true', 'false', 'null',
        'is_null', 'is_not_null', 'contains', 'starts_with', 'ends_with',
        'matches', 'not_in'
    }

    def __init__(self, rule_file_service):
        self.rule_service = rule_file_service

    def extract_dependencies(self, content: str, rule_name: str = None) -> List[str]:
        """
        Extract rule/actionset references from rule content.

        Only extracts:
        - Unquoted IDENTIFIERS
        - Double-quoted strings

        Ignores:
        - Single-quoted strings (they're literal values)
        - Keywords
        - Primitive actions
        - Attribute paths (contain dots)
        - Rule's own name
        - Common variables and attribute names

        Args:
            content: Rule content text
            rule_name: Optional name of the rule being analyzed (to exclude self-references)

        Returns:
            List of rule/actionset names referenced
        """
        dependencies = set()

        # Step 1: Remove the rule definition line FIRST to avoid matching the rule's own name
        # Handle both quoted and unquoted rule names:
        #   rule ruleName:
        #   rule "Rule Name":
        content_without_rule_def = re.sub(r'rule\s+(?:"[^"]+"|[a-zA-Z_][a-zA-Z0-9_]*)\s*:', '', content)

        # Step 2: Extract double-quoted identifiers (from remaining content after rule def removed)
        # Match anything inside double quotes
        dquoted_pattern = r'"([^"]+)"'
        dquoted_matches = re.findall(dquoted_pattern, content_without_rule_def)

        # Step 3: Extract unquoted identifiers
        # Remove all quoted strings to avoid matching words inside quotes
        content_without_quotes = re.sub(r'"[^"]+"', '', content_without_rule_def)

        # Now match word characters (won't match inside quotes anymore)
        unquoted_pattern = r'\b([a-zA-Z_][a-zA-Z0-9_]*)\b'
        unquoted_matches = re.findall(unquoted_pattern, content_without_quotes)

        # Combine all candidates (quoted + unquoted from non-quoted content)
        all_candidates = set(dquoted_matches + unquoted_matches)

        # Common variable/attribute names to exclude
        common_vars = {
            'applicant', 'transaction', 'account', 'context', 'request', 'response',
            'amount', 'creditLimit', 'creditScore', 'riskLevel', 'score', 'status',
            'merchantCode', 'employmentYears', 'monthlyIncome', 'annualIncome',
            'age', 'depth', 'condition', 'value', 'result', 'data', 'config',
            'employmentStatus', 'bankruptcyHistory', 'applicationDate', 'business_date',
            'cardCount', 'type', 'reason', 'cardNumber', 'cardType', 'monthlyIncome'
        }

        for candidate in all_candidates:
            # Skip keywords
            if candidate.lower() in self.KEYWORDS:
                continue

            # Skip primitive actions
            if candidate in self.PRIMITIVE_ACTIONS:
                continue

            # Note: We don't skip rule's own name here because:
            # 1. The rule definition line is already removed (line 73)
            # 2. We WANT to detect self-referencing cycles (rule calling itself)
            # If the rule name appears in the body, it's a legitimate dependency

            # Skip if it looks like an attribute (contains dots in original)
            # This won't catch it from regex, but we check the original content
            if f'{candidate}.' in content or f'.{candidate}' in content:
                continue

            # Skip common variable/attribute names
            if candidate in common_vars:
                continue

            # Otherwise, assume it's a rule/actionset reference
            dependencies.add(candidate)

        return sorted(list(dependencies))

    def detect_cycles(self, rule_name: str, rule_content: str) -> Dict:
        """
        Detect cycles in rule dependency graph.

        Args:
            rule_name: Name of the rule being validated
            rule_content: Content of the rule

        Returns:
            {
                'has_cycle': bool,
                'cycle_path': list[str] or None,
                'dependencies': list[str],
                'all_rules_in_chain': set[str]
            }
        """
        try:
            # Extract direct dependencies (passing rule_name to exclude self-references)
            direct_deps = self.extract_dependencies(rule_content, rule_name)

            logger.info(f"Checking cycles for rule '{rule_name}' with dependencies: {direct_deps}")

            # If no dependencies, no cycles possible
            if not direct_deps:
                return {
                    'has_cycle': False,
                    'cycle_path': None,
                    'dependencies': [],
                    'all_rules_in_chain': {rule_name}
                }

            # Build complete dependency graph
            graph = self._build_dependency_graph(rule_name, direct_deps)

            # Run DFS cycle detection
            visited = set()
            rec_stack = set()
            cycle_path = self._dfs_cycle_detection(
                rule_name, graph, visited, rec_stack, []
            )

            if cycle_path:
                logger.warning(f"Cycle detected for rule '{rule_name}': {' → '.join(cycle_path)}")

            return {
                'has_cycle': cycle_path is not None,
                'cycle_path': cycle_path,
                'dependencies': direct_deps,
                'all_rules_in_chain': visited
            }
        except Exception as e:
            logger.error(f"Error detecting cycles for rule '{rule_name}': {e}")
            # On error, assume no cycle but log the issue
            return {
                'has_cycle': False,
                'cycle_path': None,
                'dependencies': [],
                'all_rules_in_chain': {rule_name},
                'error': str(e)
            }

    def _build_dependency_graph(
        self,
        rule_name: str,
        direct_deps: List[str]
    ) -> Dict[str, List[str]]:
        """
        Build complete dependency graph including transitive dependencies.

        Args:
            rule_name: Starting rule name
            direct_deps: Direct dependencies of starting rule

        Returns:
            Dictionary mapping rule names to their dependencies
        """
        graph = {rule_name: direct_deps}
        queue = list(direct_deps)
        visited = {rule_name}

        while queue:
            current_rule = queue.pop(0)

            if current_rule in visited:
                continue
            visited.add(current_rule)

            # Load rule content
            rule_data = self._find_rule_by_name(current_rule)
            if not rule_data:
                # Rule not found, treat as leaf node
                graph[current_rule] = []
                continue

            # Extract dependencies (passing current_rule to exclude self-references)
            deps = self.extract_dependencies(rule_data.get('content', ''), current_rule)
            graph[current_rule] = deps

            # Add to queue
            for dep in deps:
                if dep not in visited:
                    queue.append(dep)

        return graph

    def _dfs_cycle_detection(
        self,
        node: str,
        graph: Dict[str, List[str]],
        visited: Set[str],
        rec_stack: Set[str],
        path: List[str]
    ) -> Optional[List[str]]:
        """
        DFS-based cycle detection.

        Args:
            node: Current node being visited
            graph: Dependency graph
            visited: Set of all visited nodes
            rec_stack: Current recursion stack
            path: Current path taken

        Returns:
            The cycle path if found, None otherwise
        """
        visited.add(node)
        rec_stack.add(node)
        path.append(node)

        # Check all dependencies
        for neighbor in graph.get(node, []):
            if neighbor not in visited:
                # Recurse
                cycle = self._dfs_cycle_detection(
                    neighbor, graph, visited, rec_stack, path
                )
                if cycle:
                    return cycle
            elif neighbor in rec_stack:
                # Cycle found!
                cycle_start_idx = path.index(neighbor)
                return path[cycle_start_idx:] + [neighbor]

        # Backtrack
        rec_stack.remove(node)
        path.pop()
        return None

    def _find_rule_by_name(self, rule_name: str) -> Optional[Dict]:
        """
        Find rule file by name across all hierarchies.

        Args:
            rule_name: Name of the rule to find

        Returns:
            Rule data dictionary or None if not found
        """
        try:
            all_rules = self.rule_service.list_rules()

            for rule in all_rules:
                if rule['name'] == rule_name:
                    # Load full rule content
                    h = rule['hierarchy']
                    return self.rule_service.get_rule(
                        h['client_code'],
                        h['process_group_code'],
                        h['process_area_code'],
                        rule['id']
                    )

            return None
        except Exception as e:
            logger.error(f"Error finding rule '{rule_name}': {e}")
            return None
