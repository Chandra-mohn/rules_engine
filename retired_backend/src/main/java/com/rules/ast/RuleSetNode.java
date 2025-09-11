package com.rules.ast;

import java.util.List;
import java.util.ArrayList;

/**
 * AST node representing a complete set of rules.
 * This is the root node of the AST tree.
 */
public class RuleSetNode extends ASTNode {
    private final List<NamedRuleNode> rules;
    
    public RuleSetNode() {
        this.rules = new ArrayList<>();
    }
    
    public RuleSetNode(List<NamedRuleNode> rules) {
        this.rules = new ArrayList<>(rules);
    }
    
    /**
     * Add a named rule to this rule set.
     * 
     * @param rule The rule to add
     */
    public void addRule(NamedRuleNode rule) {
        rules.add(rule);
    }
    
    /**
     * Get all rules in this rule set.
     * 
     * @return List of named rules
     */
    public List<NamedRuleNode> getRules() {
        return new ArrayList<>(rules);
    }
    
    /**
     * Get the number of rules in this set.
     * 
     * @return Number of rules
     */
    public int getRuleCount() {
        return rules.size();
    }
    
    /**
     * Find a rule by name.
     * 
     * @param ruleName Name of the rule to find
     * @return The named rule, or null if not found
     */
    public NamedRuleNode findRule(String ruleName) {
        return rules.stream()
                .filter(rule -> rule.getRuleName().equals(ruleName))
                .findFirst()
                .orElse(null);
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitRuleSet(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.RULE_SET;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("RuleSet[");
        sb.append("rules=").append(rules.size());
        sb.append("]");
        return sb.toString();
    }
}