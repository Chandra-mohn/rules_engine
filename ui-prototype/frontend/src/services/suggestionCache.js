/**
 * Comprehensive Suggestions Cache System
 * 
 * Preloads all autocomplete suggestions (attributes, actions, functions, etc.)
 * and provides fast in-memory lookups for rule editor autocomplete.
 */

class SuggestionCache {
  constructor() {
    this.cache = {
      loaded: false,
      loading: false,
      data: null,
      timestamp: null,
      version: null,
      error: null
    };
    
    // Cache configuration
    this.CACHE_TTL = 30 * 60 * 1000; // 30 minutes
    this.API_ENDPOINT = '/api/rules/suggestions/complete';
  }

  /**
   * Initialize and preload all suggestions
   * Call this when the rules list view loads
   */
  async preload() {
    if (this.cache.loaded || this.cache.loading) {
      return this.cache.data;
    }

    this.cache.loading = true;
    this.cache.error = null;

    try {
      console.log('🚀 Preloading suggestions cache...');
      const startTime = performance.now();
      
      const response = await fetch(this.API_ENDPOINT);
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      const data = await response.json();
      
      // Validate response structure
      if (!data || !data.attributes || !data.actions) {
        throw new Error('Invalid suggestions data structure');
      }

      this.cache.data = data;
      this.cache.loaded = true;
      this.cache.timestamp = Date.now();
      this.cache.version = data.metadata?.version || '1.0';
      
      const loadTime = Math.round(performance.now() - startTime);
      const totalCount = data.metadata?.total_count || 0;
      
      console.log(`✅ Suggestions cache loaded: ${totalCount} items in ${loadTime}ms`);
      
      return data;
    } catch (error) {
      console.error('❌ Failed to preload suggestions cache:', error);
      this.cache.error = error.message;
      this.cache.loaded = false;
      throw error;
    } finally {
      this.cache.loading = false;
    }
  }

  /**
   * Get suggestions for autocomplete based on context
   */
  getSuggestions(context = '', position = 0) {
    if (!this.cache.loaded || !this.cache.data) {
      console.warn('⚠️ Suggestions cache not loaded, returning empty array');
      return [];
    }

    const data = this.cache.data;
    const currentLine = this.extractCurrentLine(context, position);

    try {
      // Context-based suggestion filtering
      if (this.isEntityContext(currentLine, 'applicant')) {
        return data.attributes.by_entity.applicant || [];
      }
      
      if (this.isEntityContext(currentLine, 'transaction')) {
        return data.attributes.by_entity.transaction || [];
      }
      
      if (this.isEntityContext(currentLine, 'account')) {
        return data.attributes.by_entity.account || [];
      }
      
      if (this.isActionContext(currentLine)) {
        return data.actions.all || [];
      }
      
      if (this.isDateTimeContext(currentLine)) {
        return data.functions.by_category.datetime || data.functions.all || [];
      }

      // General context - return comprehensive suggestions
      return this.buildGeneralSuggestions(data);
      
    } catch (error) {
      console.error('❌ Error filtering suggestions:', error);
      return data.attributes?.all || [];
    }
  }

  /**
   * Build comprehensive suggestions list for general context
   */
  buildGeneralSuggestions(data) {
    const suggestions = [];
    
    // Add in priority order for better UX
    if (data.attributes?.all) suggestions.push(...data.attributes.all);
    if (data.actions?.all) suggestions.push(...data.actions.all);
    if (data.keywords) suggestions.push(...data.keywords);
    if (data.functions?.all) suggestions.push(...data.functions.all);
    if (data.operators) suggestions.push(...data.operators);
    if (data.time_units) suggestions.push(...data.time_units);
    
    // Remove duplicates by label
    return this.removeDuplicates(suggestions);
  }

  /**
   * Remove duplicate suggestions by label
   */
  removeDuplicates(suggestions) {
    const seen = new Set();
    return suggestions.filter(item => {
      if (seen.has(item.label)) return false;
      seen.add(item.label);
      return true;
    });
  }

  /**
   * Context detection helpers
   */
  extractCurrentLine(context, position) {
    const lines = context.substring(0, position).split('\n');
    return lines[lines.length - 1] || '';
  }

  isEntityContext(currentLine, entityName) {
    return currentLine.includes(`${entityName}.`);
  }

  isActionContext(currentLine) {
    return /\bthen\s*$/.test(currentLine.trim());
  }

  isDateTimeContext(currentLine) {
    return /\b(year_of|month_of|day_of|business_date|today|now)\b/.test(currentLine);
  }

  /**
   * Cache management methods
   */
  isExpired() {
    if (!this.cache.timestamp) return true;
    return (Date.now() - this.cache.timestamp) > this.CACHE_TTL;
  }

  clear() {
    this.cache = {
      loaded: false,
      loading: false,
      data: null,
      timestamp: null,
      version: null,
      error: null
    };
    console.log('🗑️ Suggestions cache cleared');
  }

  refresh() {
    this.clear();
    return this.preload();
  }

  /**
   * Get cache statistics for debugging
   */
  getStats() {
    if (!this.cache.data) {
      return { loaded: false, error: this.cache.error };
    }

    const data = this.cache.data;
    return {
      loaded: this.cache.loaded,
      version: this.cache.version,
      timestamp: new Date(this.cache.timestamp).toISOString(),
      totalCount: data.metadata?.total_count || 0,
      categories: {
        attributes: data.attributes?.all?.length || 0,
        actions: data.actions?.all?.length || 0,
        functions: data.functions?.all?.length || 0,
        keywords: data.keywords?.length || 0,
        operators: data.operators?.length || 0
      },
      memorySizeKB: Math.round(JSON.stringify(data).length / 1024)
    };
  }
}

// Create singleton instance
const suggestionCache = new SuggestionCache();

export default suggestionCache;