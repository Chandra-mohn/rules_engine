/**
 * Context API service for right-click help system.
 * Provides context information for ActionSets, Actions, and Attributes.
 */

import axios from 'axios';

const BASE_URL = process.env.REACT_APP_API_BASE_URL || 'http://localhost:5001';

/**
 * Get context information for a rule/actionset/action by name
 */
export const getRuleContext = async (name) => {
  try {
    const response = await axios.get(`${BASE_URL}/api/context/rule/${encodeURIComponent(name)}`);
    return response.data;
  } catch (error) {
    console.error('Failed to get rule context:', error);
    throw error;
  }
};

/**
 * Get Java source code for an action
 */
export const getActionJavaSource = async (name) => {
  try {
    const response = await axios.get(`${BASE_URL}/api/context/action/${encodeURIComponent(name)}/source`);
    return response.data;
  } catch (error) {
    console.error('Failed to get action Java source:', error);
    throw error;
  }
};

/**
 * Get schema information for an attribute
 */
export const getAttributeSchema = async (attributeName) => {
  try {
    const response = await axios.get(`${BASE_URL}/api/context/attribute/${encodeURIComponent(attributeName)}/schema`);
    return response.data;
  } catch (error) {
    console.error('Failed to get attribute schema:', error);
    throw error;
  }
};

/**
 * Determine context type from AST node or word
 */
export const determineContextType = async (word, position, editorContent) => {
  // This would ideally use AST parsing, but for MVP we'll use simple heuristics

  // Clean up the word - remove quotes if present
  const cleanWord = word.replace(/^["'`]|["'`]$/g, '');

  // Check if it looks like an attribute (contains dot)
  if (cleanWord.includes('.')) {
    return { type: 'attribute', name: cleanWord };
  }

  // Check if it's a known action/actionset by querying the backend
  try {
    const context = await getRuleContext(cleanWord);
    if (context.item_type === 'action') {
      return { type: 'action', name: cleanWord, context };
    } else if (context.item_type === 'actionset') {
      return { type: 'actionset', name: cleanWord, context };
    }
  } catch (error) {
    // Not found or error - might be something else
  }

  return { type: 'unknown', name: cleanWord };
};

export default {
  getRuleContext,
  getActionJavaSource,
  getAttributeSchema,
  determineContextType,
};