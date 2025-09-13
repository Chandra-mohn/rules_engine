import React, { useState, useEffect, useRef } from 'react';
import {
  Card,
  Form,
  Input,
  Select,
  Button,
  Space,
  message,
  Spin,
  Alert,
  Divider,
  Row,
  Col,
  Tag,
  Modal,
} from 'antd';
import {
  SaveOutlined,
  PlayCircleOutlined,
  CheckCircleOutlined,
  CloseCircleOutlined,
  ArrowLeftOutlined,
  InfoCircleOutlined,
  DatabaseOutlined,
  ThunderboltOutlined,
  CloudDownloadOutlined,
} from '@ant-design/icons';
import Editor from '@monaco-editor/react';
import { rulesApi } from '../services/api';
import { rulesLanguageDefinition, getMonacoCompletionKind } from '../utils/rulesSyntax';
import suggestionCache from '../services/suggestionCache';
import SchemaViewer from './SchemaViewer';
import SampleDataEditor from './SampleDataEditor';

const { TextArea } = Input;
const { Option } = Select;

const RuleEditor = ({ rule, onBack, onSave }) => {
  const [form] = Form.useForm();
  const [loading, setLoading] = useState(false);
  const [validating, setValidating] = useState(false);
  const [validation, setValidation] = useState(null);
  const [editorContent, setEditorContent] = useState('');
  const [schemaViewerVisible, setSchemaViewerVisible] = useState(false);
  const [sampleDataVisible, setSampleDataVisible] = useState(false);
  const [selectedSchema, setSelectedSchema] = useState('modern');
  const [parsedRuleName, setParsedRuleName] = useState('');
  const [cursorPosition, setCursorPosition] = useState({ line: 1, column: 1 });
  const editorRef = useRef(null);
  const monacoRef = useRef(null);

  // Parse rule name from content
  const parseRuleNameFromContent = (content) => {
    if (!content) return '';
    
    // Match both quoted and unquoted rule names
    // rule "PROMOTION $5%3 @SEARS":
    // rule regularRuleName:
    const ruleMatch = content.match(/^\s*rule\s+(?:"([^"]+)"|([a-zA-Z_][a-zA-Z0-9_]*))\s*:/m);
    if (ruleMatch) {
      return ruleMatch[1] || ruleMatch[2] || ''; // quoted name or unquoted name
    }
    return '';
  };

  // Update parsed rule name whenever content changes
  useEffect(() => {
    const name = parseRuleNameFromContent(editorContent);
    setParsedRuleName(name);
  }, [editorContent]);

  // Helper function to check if rule status allows execution/code generation
  const isExecutableStatus = (status) => {
    return ['VALID', 'PEND', 'SCHD', 'PROD'].includes(status);
  };

  // Initialize form and editor
  useEffect(() => {
    if (rule) {
      form.setFieldsValue({
        description: rule.description,
        status: rule.status,
      });
      setEditorContent(rule.content);
      setValidation({
        valid: rule.validation_status === 'valid',
        message: rule.validation_message,
      });
      // Set schema version from rule data or detect from content
      setSelectedSchema(rule.schema_version || detectSchemaFromContent(rule.content) || 'modern');
    } else {
      // New rule defaults
      form.setFieldsValue({
        status: 'DRAFT',
      });
      setEditorContent('rule newRule:\n    if condition then action');
    }
  }, [rule, form]);

  // Handle Monaco editor mount
  const handleEditorDidMount = (editor, monaco) => {
    editorRef.current = editor;
    monacoRef.current = monaco;

    // Track cursor position changes
    editor.onDidChangeCursorPosition((e) => {
      setCursorPosition({
        line: e.position.lineNumber,
        column: e.position.column
      });
    });

    // Register custom language
    monaco.languages.register({ id: 'rules' });
    monaco.languages.setMonarchTokensProvider('rules', rulesLanguageDefinition.monarchTokenizer);
    monaco.languages.setLanguageConfiguration('rules', rulesLanguageDefinition.languageConfiguration);
    monaco.editor.defineTheme('rules-theme', rulesLanguageDefinition.theme);
    monaco.editor.setTheme('rules-theme');

    // Register completion provider
    monaco.languages.registerCompletionItemProvider('rules', {
      provideCompletionItems: async (model, position) => {
        try {
          const textUntilPosition = model.getValueInRange({
            startLineNumber: 1,
            startColumn: 1,
            endLineNumber: position.lineNumber,
            endColumn: position.column,
          });

          // Get suggestions from cache (fast) or fallback to API (slow)
          let allSuggestions = [];
          
          try {
            // Try to get suggestions from cache first
            allSuggestions = suggestionCache.getSuggestions(textUntilPosition, textUntilPosition.length);
            
            // If cache is empty or not loaded, fall back to API
            if (!allSuggestions || allSuggestions.length === 0) {
              console.warn('Cache not available, falling back to API...');
              
              try {
                const [attributesResponse, actionsResponse] = await Promise.all([
                  fetch(`/api/schema/${selectedSchema}/attributes`),
                  fetch(`/api/schema/${selectedSchema}/actions`)
                ]);
                
                if (attributesResponse.ok && actionsResponse.ok) {
                  const attributesData = await attributesResponse.json();
                  const actionsData = await actionsResponse.json();
                  
                  allSuggestions = [
                    ...attributesData.attributes,
                    ...actionsData.actions
                  ];
                }
              } catch (apiError) {
                console.warn('API fallback failed, using empty suggestions:', apiError);
                allSuggestions = [];
              }
            }
          } catch (cacheError) {
            console.error('Error getting cached suggestions:', cacheError);
            allSuggestions = [];
          }
          
          // Filter based on current context
          const currentLine = model.getLineContent(position.lineNumber);
          const wordAtPosition = model.getWordAtPosition(position);
          const currentWord = wordAtPosition ? currentLine.substring(wordAtPosition.startColumn - 1, wordAtPosition.endColumn - 1) : '';
          
          const filteredSuggestions = allSuggestions.filter(suggestion =>
            currentWord === '' || suggestion.label.toLowerCase().includes(currentWord.toLowerCase())
          );

          const suggestions = filteredSuggestions.map(suggestion => ({
            label: suggestion.label,
            kind: getMonacoCompletionKind(suggestion.kind),
            insertText: suggestion.label,
            documentation: suggestion.documentation,
            detail: suggestion.detail,
          }));

          return { suggestions };
        } catch (error) {
          console.error('Failed to get autocomplete suggestions:', error);
          return { suggestions: [] };
        }
      },
    });

    // Set up auto-validation on content change
    let validationTimeout;
    editor.onDidChangeModelContent(() => {
      clearTimeout(validationTimeout);
      validationTimeout = setTimeout(() => {
        validateContent(editor.getValue());
      }, 1000); // Debounce validation
    });
  };

  // Convert suggestion kind to Monaco completion kind
  const getMonacoCompletionKind = (kind) => {
    const monaco = monacoRef.current;
    if (!monaco) return 1;

    switch (kind) {
      case 'keyword': return monaco.languages.CompletionItemKind.Keyword;
      case 'property': return monaco.languages.CompletionItemKind.Property;
      case 'function': return monaco.languages.CompletionItemKind.Function;
      case 'constant': return monaco.languages.CompletionItemKind.Constant;
      default: return monaco.languages.CompletionItemKind.Text;
    }
  };

  // Validate rule content
  const validateContent = async (content) => {
    if (!content.trim()) {
      setValidation(null);
      return;
    }

    setValidating(true);
    try {
      const response = await rulesApi.validateRule(content);
      setValidation(response.data);
    } catch (error) {
      setValidation({
        valid: false,
        message: 'Validation failed: ' + (error.response?.data?.error || error.message),
        errors: [error.response?.data?.error || error.message],
      });
    } finally {
      setValidating(false);
    }
  };

  // Handle manual validation
  const handleValidate = () => {
    validateContent(editorContent);
  };

  // Handle execute rule using hot compilation
  const handleExecute = async () => {
    if (!rule || !isExecutableStatus(rule.status)) {
      message.error('Rule execution is only allowed for VALID, PEND, SCHD, or PROD status rules');
      return;
    }

    try {
      if (!editorContent.trim()) {
        message.error('Please enter rule content before executing');
        return;
      }

      // Get saved sample data or use defaults
      const savedSampleData = localStorage.getItem('sampleTestData');
      let testData;

      if (savedSampleData) {
        const parsedSampleData = JSON.parse(savedSampleData);
        testData = {
          applicant: JSON.parse(parsedSampleData.applicant),
          transaction: JSON.parse(parsedSampleData.transaction),
          account: JSON.parse(parsedSampleData.account),
          metadata: {
            business_date: new Date().toISOString().split('T')[0],
            test_timestamp: new Date().toISOString()
          }
        };
      } else {
        // Use default test data (aligned with sample rules)
        testData = {
          applicant: {
            creditScore: 750,    // > 700 to trigger approve actions
            age: 28,             // >= 18 to trigger approve actions
            annualIncome: 75000,
            monthlyIncome: 6250,
            employmentStatus: "employed",
            employmentYears: 3,
            applicationDate: "2024-01-15",
            birthDate: "1995-03-22",
            requestedLimit: 5000,
            existingDebt: 12000,
            bankruptcyHistory: false,
            ssn: "123-45-6789"
          },
          transaction: {
            amount: 150.00,
            timestamp: "2024-01-15T14:30:00Z",
            merchantCategory: "5411",
            location: "US-CA-San Francisco",
            type: "purchase",
            isOnline: false
          },
          account: {
            currentBalance: 1250.00,
            creditLimit: 5000,
            availableCredit: 3750.00,
            paymentHistory: "excellent",
            accountAge: 24
          },
          metadata: {
            business_date: new Date().toISOString().split('T')[0],
            test_timestamp: new Date().toISOString()
          }
        };
      }

      setLoading(true);
      message.loading('Executing rule with hot compilation...', 0.5);
      
      // Call test API (uses hot compilation backend)
      const response = await rulesApi.testRuleContent(editorContent, testData);
      
      // Show results with execution-specific styling
      const testResult = response.data.result || response.data;
      showExecutionResults(testResult);
      
    } catch (error) {
      message.error('Execution failed: ' + (error.response?.data?.error || error.message));
    } finally {
      setLoading(false);
    }
  };

  // Handle generate production code
  const handleGenerateProductionCode = async () => {
    if (!rule || !isExecutableStatus(rule.status)) {
      message.error('Production code generation is only allowed for VALID, PEND, SCHD, or PROD status rules');
      return;
    }

    if (!editorContent.trim()) {
      message.error('Please enter rule content before generating production code');
      return;
    }

    setLoading(true);
    try {
      const ruleId = rule.id || rule.rule_id;
      if (!ruleId) {
        message.error('Rule ID not found');
        return;
      }

      const response = await rulesApi.generateProductionCode({
        ruleId: ruleId,
        ruleName: parsedRuleName || rule.name || ruleId,
        ruleContent: editorContent,
        packageName: `com.rules.generated.${ruleId.toString().toLowerCase().replace(/[^a-zA-Z0-9]/g, '')}`
      });

      if (response.data.success) {
        message.success(`Production code generated successfully! ${response.data.artifactCount} files created.`);
        
        // Display the generated files
        showGeneratedFiles(response.data);
      } else {
        message.error(`Production code generation failed: ${response.data.message || 'Unknown error'}`);
      }
    } catch (error) {
      console.error('Production code generation error:', error);
      message.error('Failed to generate production code. Please try again.');
    } finally {
      setLoading(false);
    }
  };

  // Show generated production files
  const showGeneratedFiles = (data) => {
    const { files, ruleId, ruleName, packageName } = data;
    
    Modal.info({
      title: `Production Code Generated: ${ruleName}`,
      width: '80%',
      icon: <CheckCircleOutlined style={{ color: '#52c41a' }} />,
      content: (
        <div style={{ maxHeight: '600px', overflow: 'auto' }}>
          <div style={{ marginBottom: '16px', padding: '12px', backgroundColor: '#f6ffed', border: '1px solid #b7eb8f', borderRadius: '6px' }}>
            <p><strong>Rule ID:</strong> {ruleId}</p>
            <p><strong>Package:</strong> {packageName}</p>
            <p><strong>Files Generated:</strong> {Object.keys(files).length}</p>
            <p style={{ color: '#389e0d', marginBottom: 0 }}>
              <strong>✅ Ready for version control!</strong> Copy these files to your project repository.
            </p>
          </div>
          
          {Object.entries(files).map(([filePath, content]) => (
            <div key={filePath} style={{ marginBottom: '24px' }}>
              <div style={{ 
                display: 'flex', 
                justifyContent: 'space-between', 
                alignItems: 'center',
                marginBottom: '8px',
                padding: '8px 12px',
                backgroundColor: '#f0f0f0',
                borderRadius: '4px'
              }}>
                <strong style={{ color: '#1890ff' }}>{filePath}</strong>
                <Button 
                  size="small" 
                  onClick={() => navigator.clipboard.writeText(content)}
                  icon={<SaveOutlined />}
                >
                  Copy Content
                </Button>
              </div>
              <pre style={{ 
                backgroundColor: '#fafafa', 
                border: '1px solid #d9d9d9', 
                borderRadius: '6px',
                padding: '12px',
                fontSize: '12px',
                maxHeight: '300px',
                overflow: 'auto'
              }}>
                {content}
              </pre>
            </div>
          ))}
        </div>
      ),
      okText: 'Done',
    });
  };

  // Handle test rule
  const handleTest = async () => {
    try {
      if (!editorContent.trim()) {
        message.error('Please enter rule content before testing');
        return;
      }

      // Get saved sample data or use defaults
      const savedSampleData = localStorage.getItem('sampleTestData');
      let testData;

      if (savedSampleData) {
        const parsedSampleData = JSON.parse(savedSampleData);
        testData = {
          applicant: JSON.parse(parsedSampleData.applicant),
          transaction: JSON.parse(parsedSampleData.transaction),
          account: JSON.parse(parsedSampleData.account),
          metadata: {
            business_date: new Date().toISOString().split('T')[0],
            test_timestamp: new Date().toISOString()
          }
        };
      } else {
        // Use default test data (aligned with sample rules)
        testData = {
          applicant: {
            creditScore: 750,    // > 700 to trigger approve actions
            age: 28,             // >= 18 to trigger approve actions
            annualIncome: 75000,
            monthlyIncome: 6250,
            employmentStatus: "employed",
            employmentYears: 3,
            applicationDate: "2024-01-15",
            birthDate: "1995-03-22",
            requestedLimit: 5000,
            existingDebt: 12000,
            bankruptcyHistory: false,
            ssn: "123-45-6789"
          },
          transaction: {
            amount: 150.00,
            timestamp: "2024-01-15T14:30:00Z",
            merchantCategory: "5411",
            location: "US-CA-San Francisco",
            type: "purchase",
            isOnline: false
          },
          account: {
            currentBalance: 1250.00,
            creditLimit: 5000,
            availableCredit: 3750.00,
            paymentHistory: "excellent",
            accountAge: 24
          },
          metadata: {
            business_date: new Date().toISOString().split('T')[0],
            test_timestamp: new Date().toISOString()
          }
        };
      }

      setLoading(true);
      
      // Call test API
      const response = await rulesApi.testRuleContent(editorContent, testData);
      
      // Show results - extract from nested result structure
      const testResult = response.data.result || response.data;
      showTestResults(testResult);
      
    } catch (error) {
      message.error('Test failed: ' + (error.response?.data?.error || error.message));
    } finally {
      setLoading(false);
    }
  };

  // Handle sample data
  const handleSampleData = () => {
    setSampleDataVisible(true);
  };

  // Detect schema from rule content
  const detectSchemaFromContent = (content) => {
    if (!content) return 'modern';
    
    // Look for UPPERCASE attributes (legacy indicators)
    const legacyPattern = /\b[A-Z][A-Z_]+[A-Z]\b/g;
    const modernPattern = /\b\w+\.\w+\b/g;
    
    const legacyMatches = content.match(legacyPattern) || [];
    const modernMatches = content.match(modernPattern) || [];
    
    // Filter out common keywords from legacy matches
    const keywords = ['RULE', 'IF', 'THEN', 'ELSE', 'AND', 'OR', 'NOT', 'TRUE', 'FALSE'];
    const actualLegacyMatches = legacyMatches.filter(match => !keywords.includes(match));
    
    if (actualLegacyMatches.length > 0 && modernMatches.length === 0) {
      return 'legacy';
    }
    
    return 'modern';
  };

  // Auto-detect schema when content changes
  useEffect(() => {
    if (editorContent && !rule) {
      // Only auto-detect for new rules
      const detectedSchema = detectSchemaFromContent(editorContent);
      if (detectedSchema !== selectedSchema) {
        setSelectedSchema(detectedSchema);
      }
    }
  }, [editorContent, selectedSchema, rule]);

  // Handle test with sample data
  const handleTestWithSampleData = async (testData) => {
    try {
      if (!editorContent.trim()) {
        message.error('Please enter rule content before testing');
        return;
      }

      // Call test API
      const response = await rulesApi.testRuleContent(editorContent, testData);
      
      // Show results - extract from nested result structure
      const testResult = response.data.result || response.data;
      showTestResults(testResult);
      
      // Close the sample data modal
      setSampleDataVisible(false);
    } catch (error) {
      message.error('Test failed: ' + (error.response?.data?.error || error.message));
    }
  };

  // Show execution results in a modal (similar to test but with execution styling)
  const showExecutionResults = (testResult) => {
    const { 
      success, 
      message: resultMessage, 
      ruleName, 
      executedActions = [], 
      executedActionsCount = 0,
      totalAvailableActions = 0,
      conditions = [], 
      timestamp,
      performance = {}
    } = testResult;

    Modal.success({
      title: (
        <div style={{ display: 'flex', alignItems: 'center', gap: '12px' }}>
          <span>⚡ Rule Execution Complete</span>
          <Tag color="gold" style={{ fontSize: '11px' }}>
            HOT COMPILED
          </Tag>
          {performance.method === 'hot_compilation' && (
            <Tag color="green" style={{ fontSize: '10px' }}>
              {performance.totalTimeMs}ms total
            </Tag>
          )}
        </div>
      ),
      width: 900,
      content: (
        <div>
          {/* Performance Summary */}
          {performance.method === 'hot_compilation' && (
            <Card 
              style={{ 
                marginBottom: '20px',
                background: 'linear-gradient(90deg, #f6ffed 0%, #fff7e6 100%)',
                border: '1px solid #52c41a'
              }}
            >
              <div style={{ textAlign: 'center', padding: '8px 0' }}>
                <div style={{ fontSize: '14px', fontWeight: 'bold', marginBottom: '4px', color: '#389e0d' }}>
                  🚀 High-Performance Execution
                </div>
                <div style={{ fontSize: '12px', color: '#666' }}>
                  Compilation: {performance.compilationTimeMs}ms | 
                  Execution: {performance.executionTimeMs}ms | 
                  Total: {performance.totalTimeMs}ms
                </div>
              </div>
            </Card>
          )}

          {/* Executive Summary */}
          <Card 
            style={{ 
              marginBottom: '20px',
              background: executedActionsCount > 0 ? '#f6ffed' : '#fafafa',
              border: `1px solid ${executedActionsCount > 0 ? '#b7eb8f' : '#d9d9d9'}`
            }}
          >
            <div style={{ textAlign: 'center', padding: '8px 0' }}>
              <div style={{ fontSize: '16px', fontWeight: 'bold', marginBottom: '8px' }}>
                {resultMessage}
              </div>
              <div style={{ fontSize: '14px', color: '#666' }}>
                Rule: <strong>{ruleName || 'Unnamed Rule'}</strong> | 
                Actions: {executedActionsCount} of {totalAvailableActions} executed
              </div>
            </div>
          </Card>

          {/* Actions Executed */}
          {executedActions && executedActions.length > 0 && (
            <div style={{ marginBottom: '20px' }}>
              <h4 style={{ display: 'flex', alignItems: 'center', gap: '8px', marginBottom: '12px' }}>
                <span style={{ fontSize: '18px' }}>⚡</span>
                Actions Executed ({executedActions.length})
              </h4>
              <div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
                {executedActions.map((actionItem, index) => (
                  <Card key={index} size="small" style={{ border: '1px solid #52c41a', background: '#f6ffed' }}>
                    <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start' }}>
                      <div style={{ flex: 1 }}>
                        <div style={{ display: 'flex', alignItems: 'center', gap: '8px', marginBottom: '4px' }}>
                          <Tag color="green" style={{ fontSize: '13px', fontWeight: 'bold' }}>
                            ✓ {actionItem.action}
                          </Tag>
                          <span style={{ color: '#52c41a', fontWeight: '500' }}>
                            {actionItem.description}
                          </span>
                        </div>
                        <div style={{ fontSize: '12px', color: '#666', marginLeft: '4px' }}>
                          <strong>Reason:</strong> {actionItem.reason}
                        </div>
                      </div>
                    </div>
                  </Card>
                ))}
              </div>
            </div>
          )}

          {/* No Actions Message */}
          {(!executedActions || executedActions.length === 0) && (
            <div style={{ marginBottom: '20px' }}>
              <Card style={{ textAlign: 'center', background: '#fff7e6', border: '1px solid #ffd591' }}>
                <div style={{ padding: '16px' }}>
                  <div style={{ fontSize: '16px', color: '#d48806', marginBottom: '8px' }}>
                    ⚠️ No Actions Executed
                  </div>
                  <div style={{ color: '#666' }}>
                    None of the rule conditions were met with the provided data.
                  </div>
                </div>
              </Card>
            </div>
          )}

          {/* Detailed Condition Breakdown (Collapsible) */}
          {conditions && conditions.length > 0 && (
            <details style={{ marginBottom: '16px' }}>
              <summary style={{ 
                cursor: 'pointer', 
                fontSize: '14px', 
                color: '#666',
                padding: '8px 0',
                borderBottom: '1px solid #f0f0f0'
              }}>
                <strong>🔍 View Detailed Condition Evaluation ({conditions.length} conditions)</strong>
              </summary>
              <div style={{ marginTop: '12px', maxHeight: '200px', overflow: 'auto' }}>
                {conditions.map((condition, index) => (
                  <div key={index} style={{ 
                    display: 'flex', 
                    justifyContent: 'space-between', 
                    alignItems: 'center',
                    padding: '8px 12px',
                    background: condition.evaluated ? '#f6ffed' : '#fff2f0',
                    border: `1px solid ${condition.evaluated ? '#d9f7be' : '#ffccc7'}`,
                    borderRadius: '4px',
                    marginBottom: '4px'
                  }}>
                    <code style={{ 
                      fontSize: '12px', 
                      background: 'rgba(0,0,0,0.06)', 
                      padding: '2px 6px', 
                      borderRadius: '3px',
                      flex: 1
                    }}>
                      {condition.condition}
                    </code>
                    <div style={{ marginLeft: '12px', display: 'flex', alignItems: 'center', gap: '8px' }}>
                      <Tag color={condition.evaluated ? 'green' : 'red'} size="small">
                        {condition.evaluated ? '✓ TRUE' : '✗ FALSE'}
                      </Tag>
                      {condition.executed && (
                        <Tag color="blue" size="small">→ {condition.action}</Tag>
                      )}
                    </div>
                  </div>
                ))}
              </div>
            </details>
          )}

          <div style={{ fontSize: '11px', color: '#999', textAlign: 'center', marginTop: '16px', paddingTop: '12px', borderTop: '1px solid #f0f0f0' }}>
            Execution completed at: {new Date(timestamp).toLocaleString()}
          </div>
        </div>
      ),
    });
  };

  // Show test results in a modal
  const showTestResults = (testResult) => {
    const { 
      success, 
      message: resultMessage, 
      ruleName, 
      executedActions = [], 
      executedActionsCount = 0,
      totalAvailableActions = 0,
      conditions = [], 
      timestamp 
    } = testResult;

    Modal.info({
      title: (
        <div style={{ display: 'flex', alignItems: 'center', gap: '12px' }}>
          <span>🧪 Rule Test Results</span>
          <Tag color={success ? 'green' : 'red'}>
            {success ? 'SUCCESS' : 'FAILED'}
          </Tag>
        </div>
      ),
      width: 900,
      content: (
        <div>
          {/* Executive Summary */}
          <Card 
            style={{ 
              marginBottom: '20px',
              background: success ? (executedActionsCount > 0 ? '#f6ffed' : '#fff7e6') : '#fff2f0',
              border: `1px solid ${success ? (executedActionsCount > 0 ? '#b7eb8f' : '#ffd591') : '#ffccc7'}`
            }}
          >
            <div style={{ textAlign: 'center', padding: '8px 0' }}>
              <div style={{ fontSize: '16px', fontWeight: 'bold', marginBottom: '8px' }}>
                {resultMessage}
              </div>
              <div style={{ fontSize: '14px', color: '#666' }}>
                Rule: <strong>{ruleName || 'Unnamed Rule'}</strong> | 
                Actions: {executedActionsCount} of {totalAvailableActions} will execute
              </div>
            </div>
          </Card>

          {/* Actions to Execute */}
          {executedActions && executedActions.length > 0 && (
            <div style={{ marginBottom: '20px' }}>
              <h4 style={{ display: 'flex', alignItems: 'center', gap: '8px', marginBottom: '12px' }}>
                <span style={{ fontSize: '18px' }}>⚡</span>
                Actions to Execute ({executedActions.length})
              </h4>
              <div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
                {executedActions.map((actionItem, index) => (
                  <Card key={index} size="small" style={{ border: '1px solid #1890ff', background: '#e6f7ff' }}>
                    <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start' }}>
                      <div style={{ flex: 1 }}>
                        <div style={{ display: 'flex', alignItems: 'center', gap: '8px', marginBottom: '4px' }}>
                          <Tag color="blue" style={{ fontSize: '13px', fontWeight: 'bold' }}>
                            {actionItem.action}
                          </Tag>
                          <span style={{ color: '#1890ff', fontWeight: '500' }}>
                            {actionItem.description}
                          </span>
                        </div>
                        <div style={{ fontSize: '12px', color: '#666', marginLeft: '4px' }}>
                          <strong>Reason:</strong> {actionItem.reason}
                        </div>
                      </div>
                    </div>
                  </Card>
                ))}
              </div>
            </div>
          )}

          {/* No Actions Message */}
          {(!executedActions || executedActions.length === 0) && (
            <div style={{ marginBottom: '20px' }}>
              <Card style={{ 
                textAlign: 'center', 
                background: success ? '#fff7e6' : '#fff2f0', 
                border: `1px solid ${success ? '#ffd591' : '#ffccc7'}` 
              }}>
                <div style={{ padding: '16px' }}>
                  <div style={{ 
                    fontSize: '16px', 
                    color: success ? '#d48806' : '#cf1322', 
                    marginBottom: '8px' 
                  }}>
                    {success ? 'ℹ️ No Actions Will Be Executed' : '❌ Test Failed'}
                  </div>
                  <div style={{ color: '#666' }}>
                    {success 
                      ? 'None of the rule conditions were met with the provided test data.' 
                      : 'The rule test encountered an error or validation failure.'
                    }
                  </div>
                </div>
              </Card>
            </div>
          )}

          {/* Detailed Condition Breakdown (Collapsible) */}
          {conditions && conditions.length > 0 && (
            <details style={{ marginBottom: '16px' }}>
              <summary style={{ 
                cursor: 'pointer', 
                fontSize: '14px', 
                color: '#666',
                padding: '8px 0',
                borderBottom: '1px solid #f0f0f0'
              }}>
                <strong>🔍 View Detailed Condition Evaluation ({conditions.length} conditions)</strong>
              </summary>
              <div style={{ marginTop: '12px', maxHeight: '200px', overflow: 'auto' }}>
                {conditions.map((condition, index) => (
                  <div key={index} style={{ 
                    display: 'flex', 
                    justifyContent: 'space-between', 
                    alignItems: 'center',
                    padding: '8px 12px',
                    background: condition.evaluated ? '#f6ffed' : '#fff2f0',
                    border: `1px solid ${condition.evaluated ? '#d9f7be' : '#ffccc7'}`,
                    borderRadius: '4px',
                    marginBottom: '4px'
                  }}>
                    <code style={{ 
                      fontSize: '12px', 
                      background: 'rgba(0,0,0,0.06)', 
                      padding: '2px 6px', 
                      borderRadius: '3px',
                      flex: 1
                    }}>
                      {condition.condition}
                    </code>
                    <div style={{ marginLeft: '12px', display: 'flex', alignItems: 'center', gap: '8px' }}>
                      <Tag color={condition.evaluated ? 'green' : 'red'} size="small">
                        {condition.evaluated ? '✓ TRUE' : '✗ FALSE'}
                      </Tag>
                      {condition.executed && (
                        <Tag color="blue" size="small">→ {condition.action}</Tag>
                      )}
                    </div>
                  </div>
                ))}
              </div>
            </details>
          )}

          <div style={{ fontSize: '11px', color: '#999', textAlign: 'center', marginTop: '16px', paddingTop: '12px', borderTop: '1px solid #f0f0f0' }}>
            Test executed at: {new Date(timestamp).toLocaleString()}
          </div>
        </div>
      ),
    });
  };

  // Handle save
  const handleSave = async () => {
    try {
      const values = await form.validateFields();
      
      if (!editorContent.trim()) {
        message.error('Rule content cannot be empty');
        return;
      }

      setLoading(true);

      // Validate that rule name can be parsed
      if (!parsedRuleName) {
        message.error('Could not parse rule name from content. Make sure your rule starts with "rule name:"');
        return;
      }

      // Prepare rule data - only send status if user explicitly changed it
      const ruleData = {
        description: values.description,
        name: parsedRuleName, // Use parsed name instead of form field
        content: editorContent,
        schema_version: selectedSchema,
      };

      // Only include status if it was explicitly set by user (different from initial rule status)
      if (rule && values.status !== rule.status) {
        ruleData.status = values.status;
      } else if (!rule && values.status !== 'DRAFT') {
        // For new rules, only include status if it's not the default
        ruleData.status = values.status;
      }

      let savedRule;
      if (rule) {
        // Update existing rule
        const response = await rulesApi.updateRule(rule.id, ruleData);
        savedRule = response.data;
        message.success('Rule updated successfully');
      } else {
        // Create new rule
        const response = await rulesApi.createRule(ruleData);
        savedRule = response.data;
        message.success('Rule created successfully');
      }

      // Update validation state
      if (savedRule.validation) {
        setValidation(savedRule.validation);
      }

      // Call parent callback
      if (onSave) {
        onSave(savedRule);
      }

    } catch (error) {
      if (error.response?.status === 409) {
        message.error('A rule with this name already exists');
      } else {
        message.error('Failed to save rule: ' + (error.response?.data?.error || error.message));
      }
    } finally {
      setLoading(false);
    }
  };

  return (
    <div>
      {/* Header */}
      <div style={{ marginBottom: 24, display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: 16 }}>
          <Button icon={<ArrowLeftOutlined />} onClick={onBack}>
            Back to Rules
          </Button>
          <h2>{rule ? `Edit Rule: ${parsedRuleName || rule.name}` : 'Create New Rule'}</h2>
        </div>
        <Space>
          <Button
            icon={<InfoCircleOutlined />}
            onClick={() => setSchemaViewerVisible(true)}
          >
            Schema Reference
          </Button>
          <Button
            icon={<CheckCircleOutlined />}
            onClick={handleValidate}
            loading={validating}
          >
            Validate
          </Button>
          <Button
            icon={<PlayCircleOutlined />}
            onClick={handleTest}
          >
            Test
          </Button>
          <Button
            icon={<ThunderboltOutlined />}
            onClick={handleExecute}
            disabled={!rule || !isExecutableStatus(rule.status)}
            type={rule && isExecutableStatus(rule.status) ? 'primary' : 'default'}
            style={{
              opacity: rule && isExecutableStatus(rule.status) ? 1 : 0.6
            }}
          >
            Execute {(!rule || !isExecutableStatus(rule.status)) && '(VALID+ Only)'}
          </Button>
          <Button
            icon={<CloudDownloadOutlined />}
            onClick={handleGenerateProductionCode}
            disabled={!rule || !isExecutableStatus(rule.status)}
            loading={loading}
            style={{
              opacity: rule && isExecutableStatus(rule.status) ? 1 : 0.6
            }}
          >
            Generate Production Code {(!rule || !isExecutableStatus(rule.status)) && '(VALID+ Only)'}
          </Button>
          <Button
            icon={<DatabaseOutlined />}
            onClick={handleSampleData}
          >
            Sample Data
          </Button>
          <Button
            type="primary"
            icon={<SaveOutlined />}
            onClick={handleSave}
            loading={loading}
          >
            Save Rule
          </Button>
        </Space>
      </div>

      <Row gutter={24}>
        {/* Rule Metadata */}
        <Col span={8}>
          <Card title="Rule Information" size="small">
            <Form form={form} layout="vertical">
              <Form.Item
                label="Rule Name"
                help="Extracted from rule definition"
              >
                <Input 
                  value={parsedRuleName || '(not parsed)'} 
                  disabled 
                  placeholder="Will be extracted from rule content"
                  style={{ 
                    backgroundColor: parsedRuleName ? '#f6ffed' : '#fff2f0',
                    borderColor: parsedRuleName ? '#b7eb8f' : '#ffccc7'
                  }}
                />
              </Form.Item>

              <Form.Item name="description" label="Description">
                <TextArea
                  rows={3}
                  placeholder="Enter rule description"
                />
              </Form.Item>

              <Form.Item name="status" label="Status">
                <Select>
                  <Option value="DRAFT">Draft</Option>
                  <Option value="VALID">Valid</Option>
                  <Option value="PEND">Pending Approval</Option>
                  <Option value="SCHD">Scheduled</Option>
                  <Option value="PROD">Production</Option>
                </Select>
              </Form.Item>
            </Form>

            {/* Validation Status */}
            {validation && (
              <>
                <Divider />
                <div>
                  <h4>Validation Status</h4>
                  {validation.valid ? (
                    <Alert
                      message="Valid"
                      description={validation.message}
                      type="success"
                      showIcon
                      icon={<CheckCircleOutlined />}
                    />
                  ) : (
                    <Alert
                      message="Invalid"
                      description={
                        <div>
                          <div>{validation.message}</div>
                          {validation.errors && validation.errors.length > 0 && (
                            <ul style={{ marginTop: 8, marginBottom: 0 }}>
                              {validation.errors.map((error, index) => (
                                <li key={index}>{error}</li>
                              ))}
                            </ul>
                          )}
                        </div>
                      }
                      type="error"
                      showIcon
                      icon={<CloseCircleOutlined />}
                    />
                  )}
                  {validation.warnings && validation.warnings.length > 0 && (
                    <Alert
                      style={{ marginTop: 8 }}
                      message="Warnings"
                      description={
                        <ul style={{ marginBottom: 0 }}>
                          {validation.warnings.map((warning, index) => (
                            <li key={index}>{warning}</li>
                          ))}
                        </ul>
                      }
                      type="warning"
                      showIcon
                    />
                  )}
                </div>
              </>
            )}

            {validating && (
              <>
                <Divider />
                <div style={{ textAlign: 'center' }}>
                  <Spin size="small" />
                  <span style={{ marginLeft: 8 }}>Validating...</span>
                </div>
              </>
            )}
          </Card>
        </Col>

        {/* Rule Editor */}
        <Col span={16}>
          <Card title="Rule Content" size="small">
            <div style={{ height: 600, border: '1px solid #d9d9d9', borderRadius: 6 }}>
              <Editor
                height="calc(100% - 24px)"
                language="rules"
                value={editorContent}
                onChange={setEditorContent}
                onMount={handleEditorDidMount}
                options={{
                  minimap: { enabled: false },
                  fontSize: 14,
                  lineNumbers: 'on',
                  roundedSelection: false,
                  scrollBeyondLastLine: false,
                  automaticLayout: true,
                  tabSize: 4,
                  insertSpaces: true,
                  wordWrap: 'on',
                  suggest: {
                    showKeywords: true,
                    showSnippets: true,
                  },
                }}
              />
              {/* Status Bar */}
              <div style={{
                height: 24,
                backgroundColor: '#f5f5f5',
                borderTop: '1px solid #d9d9d9',
                display: 'flex',
                alignItems: 'center',
                paddingLeft: 12,
                fontSize: 12,
                color: '#666',
                borderBottomLeftRadius: 6,
                borderBottomRightRadius: 6,
              }}>
                Line {cursorPosition.line}, Column {cursorPosition.column}
              </div>
            </div>

            {/* Syntax Help */}
            <div style={{ marginTop: 16 }}>
              <h4>Quick Reference</h4>
              <div style={{ display: 'flex', flexWrap: 'wrap', gap: 8 }}>
                <Tag>rule name: if condition then action</Tag>
                <Tag>applicant.creditScore >= 700</Tag>
                <Tag>business_date + 30 days</Tag>
                <Tag>day_of_week(timestamp) >= 6</Tag>
                <Tag>approveApplication</Tag>
                <Tag>and, or, not</Tag>
              </div>
            </div>
          </Card>
        </Col>
      </Row>

      {/* Schema Viewer Modal */}
      <SchemaViewer
        visible={schemaViewerVisible}
        onClose={() => setSchemaViewerVisible(false)}
        schemaVersion={selectedSchema}
      />

      {/* Sample Data Editor Modal */}
      <SampleDataEditor
        visible={sampleDataVisible}
        onClose={() => setSampleDataVisible(false)}
        onTest={handleTestWithSampleData}
        currentRuleContent={editorContent}
      />
    </div>
  );
};

export default RuleEditor;