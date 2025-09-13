package com.rules.runtime;

import com.rules.engine.Rule;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Custom ClassLoader for dynamic rule loading and hot-swapping.
 * Loads compiled rule classes from bytecode without file system writes.
 */
public class HotRuleClassLoader extends ClassLoader {
    
    private final ConcurrentHashMap<String, byte[]> classBytecode = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Class<?>> loadedClasses = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Long> classVersions = new ConcurrentHashMap<>();
    private final AtomicLong versionCounter = new AtomicLong(0);
    
    public HotRuleClassLoader() {
        super(HotRuleClassLoader.class.getClassLoader());
    }
    
    /**
     * Define a new rule class from bytecode
     * @param className Fully qualified class name
     * @param bytecode Compiled Java bytecode
     * @return The loaded Class object
     */
    public Class<?> defineRuleClass(String className, byte[] bytecode) {
        // Store bytecode for future reference
        classBytecode.put(className, bytecode.clone());
        
        // Generate new version for this class
        long version = versionCounter.incrementAndGet();
        classVersions.put(className, version);
        
        // Define the class
        Class<?> clazz = defineClass(className, bytecode, 0, bytecode.length);
        
        // Cache the loaded class
        loadedClasses.put(className, clazz);
        
        return clazz;
    }
    
    /**
     * Hot-swap an existing rule class with new bytecode
     * @param className Fully qualified class name
     * @param newBytecode Updated compiled Java bytecode
     * @return The new Class object
     */
    public Class<?> redefineRuleClass(String className, byte[] newBytecode) {
        // Remove old class from cache
        loadedClasses.remove(className);
        
        // Create new version with updated bytecode
        return defineRuleClass(className, newBytecode);
    }
    
    /**
     * Get a loaded rule class by name
     * @param className Fully qualified class name
     * @return The Class object, or null if not found
     */
    public Class<?> getRuleClass(String className) {
        return loadedClasses.get(className);
    }
    
    /**
     * Create an instance of a loaded rule class
     * @param className Fully qualified class name
     * @return New Rule instance
     * @throws Exception If class not found or instantiation fails
     */
    public Rule createRuleInstance(String className) throws Exception {
        Class<?> clazz = getRuleClass(className);
        if (clazz == null) {
            throw new ClassNotFoundException("Rule class not found: " + className);
        }
        
        if (!Rule.class.isAssignableFrom(clazz)) {
            throw new IllegalArgumentException("Class " + className + " does not implement Rule interface");
        }
        
        return (Rule) clazz.getDeclaredConstructor().newInstance();
    }
    
    /**
     * Check if a rule class is loaded
     * @param className Fully qualified class name
     * @return true if loaded, false otherwise
     */
    public boolean isClassLoaded(String className) {
        return loadedClasses.containsKey(className);
    }
    
    /**
     * Get the version of a loaded class
     * @param className Fully qualified class name
     * @return Version number, or -1 if not found
     */
    public long getClassVersion(String className) {
        return classVersions.getOrDefault(className, -1L);
    }
    
    /**
     * Remove a rule class from the loader
     * @param className Fully qualified class name
     * @return true if removed, false if not found
     */
    public boolean unloadClass(String className) {
        boolean removed = false;
        
        if (loadedClasses.remove(className) != null) {
            removed = true;
        }
        
        if (classBytecode.remove(className) != null) {
            removed = true;
        }
        
        if (classVersions.remove(className) != null) {
            removed = true;
        }
        
        return removed;
    }
    
    /**
     * Get statistics about loaded classes
     * @return ClassLoaderStats object with current state
     */
    public ClassLoaderStats getStats() {
        return new ClassLoaderStats(
            loadedClasses.size(),
            classBytecode.values().stream().mapToInt(bytes -> bytes.length).sum(),
            versionCounter.get()
        );
    }
    
    /**
     * Statistics about the class loader state
     */
    public static class ClassLoaderStats {
        public final int loadedClassCount;
        public final int totalBytecodeSize;
        public final long highestVersion;
        
        public ClassLoaderStats(int loadedClassCount, int totalBytecodeSize, long highestVersion) {
            this.loadedClassCount = loadedClassCount;
            this.totalBytecodeSize = totalBytecodeSize;
            this.highestVersion = highestVersion;
        }
        
        @Override
        public String toString() {
            return String.format("ClassLoaderStats{classes=%d, bytecode=%d bytes, version=%d}", 
                loadedClassCount, totalBytecodeSize, highestVersion);
        }
    }
    
    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
        // Check if we have bytecode for this class
        byte[] bytecode = classBytecode.get(name);
        if (bytecode != null) {
            return defineClass(name, bytecode, 0, bytecode.length);
        }
        
        // Fall back to parent class loader
        return super.findClass(name);
    }
}