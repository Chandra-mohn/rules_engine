package com.rules;

import org.json.JSONObject;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.lang.reflect.Method;
import java.util.List;
import java.util.ArrayList;

/**
 * Minimal rule executor for testing generated rule classes.
 * Loads and executes compiled rule classes with test data.
 */
public class RuleExecutor {

    public static void main(String[] args) {
        if (args.length < 2) {
            System.err.println("Usage: RuleExecutor <className> <testDataFile>");
            System.exit(1);
        }

        String className = args[0];
        String testDataFile = args[1];

        try {
            // Load test data
            String testDataJson = new String(Files.readAllBytes(Paths.get(testDataFile)));
            JSONObject testData = new JSONObject(testDataJson);

            // Load and instantiate rule class
            Class<?> ruleClass = Class.forName(className);
            Object ruleInstance = ruleClass.getDeclaredConstructor().newInstance();

            // Execute rule - look for evaluate method
            Method evaluateMethod = ruleClass.getMethod("evaluate", JSONObject.class);
            Object result = evaluateMethod.invoke(ruleInstance, testData);

            // Format result for output
            JSONObject output = new JSONObject();

            if (result instanceof Boolean) {
                output.put("matched", (Boolean) result);
                output.put("actions", new ArrayList<String>());
                output.put("finalAction", null);
            } else if (result instanceof JSONObject) {
                // Result is already formatted
                System.out.println(result.toString());
                return;
            } else {
                // Assume it's an action result
                output.put("matched", true);
                List<String> actions = new ArrayList<>();
                if (result != null) {
                    actions.add(result.toString());
                    output.put("finalAction", result.toString());
                }
                output.put("actions", actions);
            }

            System.out.println(output.toString());

        } catch (Exception e) {
            // Error output as JSON
            JSONObject error = new JSONObject();
            error.put("error", e.getMessage());
            error.put("matched", false);
            error.put("actions", new ArrayList<String>());
            System.err.println(error.toString());
            System.exit(1);
        }
    }
}