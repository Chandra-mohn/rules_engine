package com.rules.context;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;

/**
 * Rule execution context containing all available data.
 * Provides efficient access to JSON data without copying.
 */
public class RuleContext {
    private final JsonNode data;
    private final ObjectMapper objectMapper;
    
    public RuleContext(String jsonData) throws IOException {
        this.objectMapper = new ObjectMapper();
        this.data = objectMapper.readTree(jsonData);
    }
    
    public RuleContext(JsonNode jsonNode) {
        this.objectMapper = new ObjectMapper();
        this.data = jsonNode;
    }
    
    /**
     * Get value at specified JSON path.
     * 
     * @param path Dot-separated path (e.g., "customer.age")
     * @return Value at path or null if not found
     */
    public Object getValue(String path) {
        JsonNode node = getJsonNode(path);
        if (node == null || node.isNull()) {
            return null;
        }
        
        if (node.isTextual()) {
            return node.asText();
        } else if (node.isInt()) {
            return node.asInt();
        } else if (node.isLong()) {
            return node.asLong();
        } else if (node.isDouble()) {
            return node.asDouble();
        } else if (node.isBoolean()) {
            return node.asBoolean();
        }
        
        return node;
    }
    
    /**
     * Get string value at specified path.
     */
    public String getString(String path) {
        JsonNode node = getJsonNode(path);
        return node != null ? node.asText() : null;
    }
    
    /**
     * Get integer value at specified path.
     */
    public Integer getInteger(String path) {
        JsonNode node = getJsonNode(path);
        return node != null && !node.isNull() ? node.asInt() : null;
    }
    
    /**
     * Get double value at specified path.
     */
    public Double getDouble(String path) {
        JsonNode node = getJsonNode(path);
        return node != null && !node.isNull() ? node.asDouble() : null;
    }
    
    /**
     * Get boolean value at specified path.
     */
    public Boolean getBoolean(String path) {
        JsonNode node = getJsonNode(path);
        return node != null && !node.isNull() ? node.asBoolean() : null;
    }
    
    /**
     * Get the complete JSON data.
     */
    public JsonNode getData() {
        return data;
    }
    
    private JsonNode getJsonNode(String path) {
        if (path == null || path.isEmpty()) {
            return null;
        }
        
        String[] parts = path.split("\\.");
        JsonNode current = data;
        
        for (String part : parts) {
            if (current == null) {
                return null;
            }
            current = current.get(part);
        }
        
        return current;
    }
}