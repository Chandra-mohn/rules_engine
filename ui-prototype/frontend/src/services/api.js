import axios from 'axios';

// Create axios instance with base configuration
const api = axios.create({
  baseURL: process.env.REACT_APP_API_URL || 'http://localhost:5001/api',
  timeout: 30000,
  headers: {
    'Content-Type': 'application/json',
  },
});

// Request interceptor to add user ID header
api.interceptors.request.use(
  (config) => {
    // Add user ID header (in a real app, this would come from authentication)
    config.headers['X-User-ID'] = 'demo-user';
    return config;
  },
  (error) => {
    return Promise.reject(error);
  }
);

// Response interceptor for error handling
api.interceptors.response.use(
  (response) => response,
  (error) => {
    console.error('API Error:', error.response?.data || error.message);
    return Promise.reject(error);
  }
);

// Rules API
export const rulesApi = {
  // Get all rules with pagination and filtering
  getRules: (params = {}) => {
    return api.get('/rules', { params });
  },

  // Get a specific rule by ID
  getRule: (id) => {
    return api.get(`/rules/${id}`);
  },

  // Create a new rule
  createRule: (ruleData) => {
    return api.post('/rules', ruleData);
  },

  // Update an existing rule
  updateRule: (id, ruleData) => {
    return api.put(`/rules/${id}`, ruleData);
  },

  // Delete a rule
  deleteRule: (id) => {
    return api.delete(`/rules/${id}`);
  },

  // Validate rule syntax
  validateRule: (content) => {
    return api.post('/rules/validate', { content });
  },

  // Test rule execution
  testRule: (id, testData, ruleContent = null) => {
    return api.post(`/rules/${id}/test`, {
      test_data: testData,
      rule_content: ruleContent,
    });
  },

  // Get autocomplete suggestions
  getAutocompleteSuggestions: (context, position) => {
    return api.post('/rules/autocomplete', { context, position });
  },

  // Get rule history
  getRuleHistory: (id) => {
    return api.get(`/rules/${id}/history`);
  },

  // Revert rule to previous version
  revertRule: (id, version) => {
    return api.post(`/rules/${id}/revert/${version}`);
  },
};

// Schema API
export const schemaApi = {
  // Get complete schema
  getFullSchema: () => {
    return api.get('/schema');
  },

  // Get all attributes
  getAttributes: () => {
    return api.get('/schema/attributes');
  },

  // Get attributes for specific entity
  getEntityAttributes: (entity) => {
    return api.get(`/schema/attributes/${entity}`);
  },

  // Get all actions
  getActions: () => {
    return api.get('/schema/actions');
  },

  // Get all functions
  getFunctions: () => {
    return api.get('/schema/functions');
  },

  // Get keywords and operators
  getKeywords: () => {
    return api.get('/schema/keywords');
  },
};

// Health check
export const healthCheck = () => {
  return api.get('/health');
};

export default api;