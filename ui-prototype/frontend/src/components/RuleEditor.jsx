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
  Statistic,
} from 'antd';
import {
  CheckCircleOutlined,
  CloseCircleOutlined,
  ArrowLeftOutlined,
  CloudDownloadOutlined,
  ToolOutlined,
  InfoCircleOutlined,
  SaveOutlined,
  WarningOutlined,
} from '@ant-design/icons';
import Editor from '@monaco-editor/react';
import { rulesApi, contextsApi } from '../services/api';
import { rulesLanguageDefinition } from '../utils/rulesSyntax';
import suggestionCache from '../services/suggestionCache';
import SchemaViewer from './SchemaViewer';
import ContextManager from './ContextManager';
import ContextPanel from './ContextPanel';
import { getRuleContext, getActionJavaSource, getAttributeSchema, determineContextType } from '../services/contextApi';

const { TextArea } = Input;
const { Option } = Select;

const RuleEditor = ({ rule, onBack, onSave }) => {
  const [form] = Form.useForm();
  const [loading, setLoading] = useState(false);
  const [validating, setValidating] = useState(false);
  const [validation, setValidation] = useState(null);
  const [editorContent, setEditorContent] = useState('');
  const [schemaViewerVisible, setSchemaViewerVisible] = useState(false);
  const [selectedSchema, setSelectedSchema] = useState('modern');
  const [parsedRuleName, setParsedRuleName] = useState('');
  const [cursorPosition, setCursorPosition] = useState({ line: 1, column: 1 });
  const [processAreas, setProcessAreas] = useState([]);
  const [loadingProcessAreas, setLoadingProcessAreas] = useState(false);
  const [panelMode, setPanelMode] = useState('validation'); // 'validation' or 'context'
  const [contextInfo, setContextInfo] = useState(null);
  const [loadingContext, setLoadingContext] = useState(false);
  const [contexts, setContexts] = useState([]);
  const [loadingContexts, setLoadingContexts] = useState(false);
  const [contextManagerVisible, setContextManagerVisible] = useState(false);
  const [contextPanelVisible, setContextPanelVisible] = useState(() => {
    // Load panel state from localStorage
    const saved = localStorage.getItem('contextPanelVisible');
    return saved !== null ? JSON.parse(saved) : false;
  });
  const editorRef = useRef(null);
  const monacoRef = useRef(null);

  // Parse rule name from content
  const parseRuleNameFromContent = (content) => {
    if (!content) return '';

    // Match both quoted and unquoted rule names
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

  // Load process areas for the dropdown
  const loadProcessAreas = async () => {
    setLoadingProcessAreas(true);
    try {
      const response = await rulesApi.getAllProcessAreas();
      setProcessAreas(response.data.process_areas || []);
    } catch (error) {
      console.error('Failed to load process areas:', error);
      message.error('Failed to load process areas');
    } finally {
      setLoadingProcessAreas(false);
    }
  };

  // Load contexts for the dropdown
  const loadContexts = async () => {
    setLoadingContexts(true);
    try {
      const response = await contextsApi.getContexts({ limit: 100 });
      setContexts(response.data.contexts || []);
    } catch (error) {
      console.error('Failed to load contexts:', error);
      message.error('Failed to load contexts');
    } finally {
      setLoadingContexts(false);
    }
  };

  // Initialize form and editor
  useEffect(() => {
    if (rule) {
      const formValues = {
        description: rule.description,
        status: rule.status,
        process_area_id: rule.process_area_id,
        context_id: rule.context_id,
      };

      // Add name field for Actions and ActionSets
      if (rule.item_type === 'action' || rule.item_type === 'actionset') {
        formValues.name = rule.name;
      }

      console.log('Setting form values, context_id:', rule.context_id);
      form.setFieldsValue(formValues);
      setEditorContent(rule.content || '');
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

      // Set default content based on item type
      if (rule?.item_type === 'action') {
        setEditorContent(JSON.stringify({
          "javaPath": "actions/NewAction.java",
          "parameters": [
            {"name": "param1", "type": "string", "required": true}
          ]
        }, null, 2));
      } else {
        setEditorContent('rule newRule:\n    if condition then action');
      }
    }
  }, [rule, form]);

  // Load process areas and contexts on component mount
  useEffect(() => {
    loadProcessAreas();
    loadContexts();
  }, []);

  // Re-set context_id when contexts finish loading (to ensure dropdown has the option available)
  useEffect(() => {
    if (rule && rule.context_id && contexts.length > 0 && !loadingContexts) {
      console.log('Re-setting context_id after contexts loaded:', rule.context_id);
      console.log('Available contexts:', contexts.map(c => ({ id: c.id, name: c.name })));
      form.setFieldsValue({ context_id: rule.context_id });
    }
  }, [contexts, loadingContexts, rule, form]);

  // Auto-show/hide panel based on context selection and persist to localStorage
  useEffect(() => {
    const contextId = form.getFieldValue('context_id');
    if (contextId && !contextPanelVisible) {
      setContextPanelVisible(true);
    }
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [contextPanelVisible]);

  // Persist panel visibility to localStorage
  useEffect(() => {
    localStorage.setItem('contextPanelVisible', JSON.stringify(contextPanelVisible));
  }, [contextPanelVisible]);

  // Load context information for a word
  const loadContextInfo = async (word, position, editorContent) => {
    try {
      setLoadingContext(true);
      setPanelMode('context');

      // Determine context type using the context API service
      const contextType = await determineContextType(word, position, editorContent);

      let contextData = null;

      switch (contextType.type) {
        case 'action':
        case 'actionset':
          // Get rule context
          contextData = await getRuleContext(word);

          // If it's an action, also get the Java source
          if (contextData.item_type === 'action' && contextData.java_file_path) {
            try {
              const javaSource = await getActionJavaSource(word);
              contextData.javaSource = javaSource;
            } catch (error) {
              console.warn('Could not load Java source for action:', word, error);
            }
          }
          break;

        case 'attribute':
          // Get attribute schema
          contextData = await getAttributeSchema(word);
          break;

        default:
          contextData = {
            type: 'unknown',
            name: word,
            message: `No context information available for "${word}"`
          };
      }

      setContextInfo(contextData);

    } catch (error) {
      console.error('Failed to load context info:', error);
      setContextInfo({
        type: 'error',
        name: word,
        message: 'Failed to load context information',
        error: error.message
      });
    } finally {
      setLoadingContext(false);
    }
  };

  // Extract complete context word (handles compound identifiers and quoted strings)
  const extractContextWord = (model, position) => {
    const lineContent = model.getLineContent(position.lineNumber);
    const column = position.column;

    // Check if we're in a quoted string first
    const quotedString = extractQuotedString(lineContent, column);
    if (quotedString) {
      return quotedString;
    }

    // Always try our compound identifier extraction first (handles camelCase, snake_case, dot notation)
    const compoundIdentifier = extractCompoundIdentifier(lineContent, column);
    if (compoundIdentifier) {
      return compoundIdentifier;
    }

    // Only fall back to Monaco if our extraction found nothing
    const word = model.getWordAtPosition(position);
    return word ? word.word : null;
  };

  // Extract quoted strings (handles both single and double quotes)
  const extractQuotedString = (line, column) => {
    // Check for quoted strings with various quote types
    const quotePatterns = [
      { start: '"', end: '"' },
      { start: "'", end: "'" },
      { start: '`', end: '`' }
    ];

    for (const pattern of quotePatterns) {
      // Find the start quote before the cursor
      let startQuote = -1;
      for (let i = column - 1; i >= 0; i--) {
        if (line[i] === pattern.start) {
          startQuote = i;
          break;
        }
        // If we hit another quote type or newline, break
        if (line[i] === pattern.end && i < column - 1) break;
      }

      if (startQuote >= 0) {
        // Find the end quote after the cursor
        let endQuote = -1;
        for (let i = column; i < line.length; i++) {
          if (line[i] === pattern.end) {
            endQuote = i;
            break;
          }
        }

        if (endQuote > startQuote) {
          // Extract the quoted content including quotes
          return line.substring(startQuote, endQuote + 1);
        }
      }
    }

    return null;
  };

  // Extract compound identifiers (e.g., applicant.creditScore, business_date, CREDIT_SCORE_THRESHOLD, approveApplication)
  const extractCompoundIdentifier = (line, column) => {
    // Define what constitutes a valid identifier character (including camelCase)
    const isIdentifierChar = (char) => {
      return /[a-zA-Z0-9_]/.test(char);
    };

    const isDotChar = (char) => {
      return char === '.';
    };

    // Find the start of the identifier
    let start = column - 1;
    while (start >= 0 && (isIdentifierChar(line[start]) || isDotChar(line[start]))) {
      start--;
    }
    start++; // Move to first valid character

    // Find the end of the identifier
    let end = column - 1;
    while (end < line.length && (isIdentifierChar(line[end]) || isDotChar(line[end]))) {
      end++;
    }

    // Extract the identifier
    if (end > start) {
      const identifier = line.substring(start, end);

      // Validate it's a reasonable identifier (not just dots, at least one alphanumeric)
      if (/[a-zA-Z0-9]/.test(identifier) && identifier.length > 0) {
        return identifier;
      }
    }

    return null;
  };

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

    // Add right-click context menu
    editor.onContextMenu((e) => {
      // Get word at cursor position
      const position = e.target.position;
      if (!position) return;

      const model = editor.getModel();
      const extractedWord = extractContextWord(model, position);

      if (extractedWord && extractedWord.trim()) {
        // Load context info for the extracted word
        loadContextInfo(extractedWord, position, model.getValue());
      }
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

          // Get context-aware suggestions from backend ANTLR analysis
          let allSuggestions = [];

          try {
            // Use backend autocomplete API for intelligent context analysis
            const autocompleteResponse = await fetch('/api/rules/autocomplete', {
              method: 'POST',
              headers: {
                'Content-Type': 'application/json'
              },
              body: JSON.stringify({
                context: textUntilPosition,
                position: textUntilPosition.length
              })
            });

            if (autocompleteResponse.ok) {
              const autocompleteData = await autocompleteResponse.json();
              allSuggestions = autocompleteData.suggestions || [];

              // Log context info for debugging
              console.log('Autocomplete context:', autocompleteData.context_type, autocompleteData.cursor_info);
            } else {
              // Fallback to cache if API fails
              allSuggestions = suggestionCache.getSuggestions(textUntilPosition, textUntilPosition.length);

              // If still empty, fallback to basic schema API
              if (!allSuggestions || allSuggestions.length === 0) {
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
                } catch (fallbackError) {
                  console.error('Fallback API error:', fallbackError);
                  allSuggestions = [];
                }
              }
            }
          } catch (error) {
            console.error('Error getting autocomplete suggestions:', error);
            // Final fallback to cache
            allSuggestions = suggestionCache.getSuggestions(textUntilPosition, textUntilPosition.length) || [];
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

    // Register hover provider for error details
    monaco.languages.registerHoverProvider('rules', {
      provideHover: function(model, position) {
        // Check if there's a marker at this position
        const markers = monaco.editor.getModelMarkers({ resource: model.uri, owner: 'rules-validator' });
        const marker = markers.find(m =>
          m.startLineNumber === position.lineNumber &&
          position.column >= m.startColumn &&
          position.column <= m.endColumn
        );

        if (marker) {
          return {
            contents: [
              { value: `**${marker.severity === monaco.MarkerSeverity.Error ? '🔴 Error' : '⚠️ Warning'}**` },
              { value: marker.message }
            ]
          };
        }

        return null;
      }
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
      // Clear markers when no content
      if (editorRef.current && monacoRef.current) {
        monacoRef.current.editor.setModelMarkers(
          editorRef.current.getModel(),
          'rules-validator',
          []
        );
      }
      return;
    }

    setValidating(true);
    try {
      const response = await rulesApi.validateRule(content);
      setValidation(response.data);

      // Add Monaco markers for errors
      if (editorRef.current && monacoRef.current) {
        updateMonacoMarkers(response.data);
      }
    } catch (error) {
      setValidation({
        valid: false,
        message: 'Validation failed: ' + (error.response?.data?.error || error.message),
        errors: [error.response?.data?.error || error.message],
      });

      // Clear markers on exception
      if (editorRef.current && monacoRef.current) {
        monacoRef.current.editor.setModelMarkers(
          editorRef.current.getModel(),
          'rules-validator',
          []
        );
      }
    } finally {
      setValidating(false);
    }
  };

  // Update Monaco markers based on validation results
  const updateMonacoMarkers = (validationData) => {
    if (!editorRef.current || !monacoRef.current) return;

    const monaco = monacoRef.current;
    const model = editorRef.current.getModel();
    const markers = [];

    // Add markers for errors
    if (validationData.errors && validationData.errors.length > 0) {
      validationData.errors.forEach(err => {
        const severity = err.severity === 'error' || err.type === 'grammar'
          ? monaco.MarkerSeverity.Error
          : monaco.MarkerSeverity.Warning;

        const marker = {
          severity: severity,
          startLineNumber: err.line || 1,
          startColumn: err.column || 1,
          endLineNumber: err.line || 1,
          endColumn: (err.column || 1) + (err.length || 1),
          message: err.message || 'Validation error',
        };

        // Don't show available actions list - keep error messages clean and concise
        // Users can see available actions through autocomplete

        markers.push(marker);
      });
    }

    // Set markers
    monaco.editor.setModelMarkers(model, 'rules-validator', markers);
  };

  // Handle manual validation
  const handleValidate = () => {
    validateContent(editorContent);
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
        packageName: `com.rules.${ruleId.toString().toLowerCase().replace(/[^a-zA-Z0-9]/g, '')}`,
        itemType: rule.item_type || 'rule'
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
    const { files, ruleId, ruleName, packageName, outputDirectory } = data;

    Modal.info({
      title: `Code Generated: ${ruleName}`,
      width: '600px',
      icon: <CheckCircleOutlined style={{ color: '#52c41a' }} />,
      content: (
        <div>
          <div style={{ marginBottom: '20px', padding: '16px', backgroundColor: '#f6ffed', border: '1px solid #b7eb8f', borderRadius: '6px' }}>
            <div style={{ marginBottom: '12px' }}>
              <strong style={{ color: '#389e0d', fontSize: '16px' }}>✅ Code Generated Successfully!</strong>
            </div>
            <div style={{ marginBottom: '8px' }}>
              <strong>Rule ID:</strong> {ruleId}
            </div>
            <div style={{ marginBottom: '8px' }}>
              <strong>Package:</strong> {packageName}
            </div>
            <div style={{ marginBottom: '12px' }}>
              <strong>Files Generated:</strong> {Object.keys(files).length}
            </div>
            <div style={{
              padding: '12px',
              backgroundColor: '#ffffff',
              border: '1px solid #d9d9d9',
              borderRadius: '4px',
              fontSize: '13px',
              fontFamily: 'monospace',
              color: '#1890ff',
              wordBreak: 'break-all'
            }}>
              <strong>📂 Location:</strong> {outputDirectory}
            </div>
          </div>

          <div style={{ textAlign: 'center', color: '#666', fontSize: '14px' }}>
            Generated files are saved to the above directory and ready for version control.
          </div>
        </div>
      ),
      okText: 'Done',
    });
  };

  // Handle test code - build and test generated Java code using Maven
  const handleTestCode = async () => {
    if (!rule || !rule.id) {
      message.error('Please save the rule first before testing generated code');
      return;
    }

    if (!isExecutableStatus(rule.status)) {
      message.error('Code testing is only allowed for VALID, PEND, SCHD, or PROD status rules');
      return;
    }

    setLoading(true);

    try {
      const ruleId = rule.id || rule.rule_id;

      // Step 1: Build the code
      message.loading('Building Java code with Maven...', 0);

      try {
        const buildResponse = await rulesApi.buildRuleCode(ruleId);

        if (!buildResponse.data.success) {
          message.destroy();
          showTestCodeResults({
            success: false,
            phase: 'build',
            error: buildResponse.data.error,
            buildLog: buildResponse.data.build_log,
            buildTime: buildResponse.data.build_time_ms
          });
          return;
        }

        // Step 2: Run tests
        message.loading('Running Maven tests...', 0);

        const testResponse = await rulesApi.testRuleCode(ruleId);

        message.destroy();
        showTestCodeResults({
          success: testResponse.data.success,
          phase: testResponse.data.success ? 'complete' : 'test',
          buildResult: buildResponse.data,
          testResult: testResponse.data,
          error: testResponse.data.error
        });

      } catch (apiError) {
        message.destroy();
        console.error('Maven test error:', apiError);
        showTestCodeResults({
          success: false,
          phase: 'api',
          error: apiError.response?.data?.error || apiError.message,
          buildLog: apiError.response?.data?.build_log || '',
          testLog: apiError.response?.data?.test_log || ''
        });
      }

    } catch (error) {
      message.destroy();
      console.error('Test code error:', error);
      message.error('Failed to test generated code: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  // Show test code results
  const showTestCodeResults = (data) => {
    const { success, phase, buildResult, testResult, error, buildLog, testLog } = data;

    const getStatusIcon = () => {
      if (success) return <CheckCircleOutlined style={{ color: '#52c41a' }} />;
      return <CloseCircleOutlined style={{ color: '#ff4d4f' }} />;
    };

    const getPhaseMessage = () => {
      switch (phase) {
        case 'build': return 'Build Failed';
        case 'test': return 'Tests Failed';
        case 'complete': return 'Build and Tests Passed';
        case 'api': return 'API Error';
        default: return 'Test Incomplete';
      }
    };

    Modal.info({
      title: `Code Test Results: ${getPhaseMessage()}`,
      width: '700px',
      icon: getStatusIcon(),
      content: (
        <div>
          {/* Status Summary */}
          <div style={{
            marginBottom: '20px',
            padding: '16px',
            backgroundColor: success ? '#f6ffed' : '#fff2f0',
            border: `1px solid ${success ? '#b7eb8f' : '#ffccc7'}`,
            borderRadius: '6px'
          }}>
            <div style={{ marginBottom: '12px' }}>
              <strong style={{
                color: success ? '#389e0d' : '#cf1322',
                fontSize: '16px'
              }}>
                {success ? '✅ All Tests Passed!' : '❌ ' + getPhaseMessage()}
              </strong>
            </div>

            {/* Build Results */}
            {buildResult && (
              <div style={{ marginBottom: '8px' }}>
                <strong>Build:</strong> {buildResult.success ? '✅ Success' : '❌ Failed'}
                {buildResult.build_time_ms && ` (${buildResult.build_time_ms}ms)`}
              </div>
            )}

            {/* Test Results */}
            {testResult && (
              <div style={{ marginBottom: '8px' }}>
                <strong>Tests:</strong> {testResult.success ? '✅ All Passed' : '❌ Failed'}
                {testResult.test_time_ms && ` (${testResult.test_time_ms}ms)`}
              </div>
            )}

            {/* Test Summary */}
            {testResult?.test_summary && (
              <div style={{ marginBottom: '8px', fontSize: '13px' }}>
                <strong>Summary:</strong> {testResult.test_summary.tests_run} tests,
                {testResult.test_summary.failures} failures,
                {testResult.test_summary.errors} errors
              </div>
            )}

            {/* Error Message */}
            {error && (
              <div style={{
                marginTop: '12px',
                padding: '8px',
                backgroundColor: '#fff2f0',
                border: '1px solid #ffccc7',
                borderRadius: '4px',
                fontSize: '13px',
                color: '#cf1322'
              }}>
                <strong>Error:</strong> {error}
              </div>
            )}
          </div>

          {/* Build/Test Logs */}
          {(buildLog || testLog || buildResult?.build_log || testResult?.test_log) && (
            <div>
              <h4>Log Output:</h4>
              <div style={{
                maxHeight: '300px',
                overflow: 'auto',
                padding: '12px',
                backgroundColor: '#f5f5f5',
                border: '1px solid #d9d9d9',
                borderRadius: '4px',
                fontSize: '12px',
                fontFamily: 'monospace',
                whiteSpace: 'pre-line'
              }}>
                {buildResult?.build_log || buildLog || ''}
                {testResult?.test_log || testLog || ''}
              </div>
            </div>
          )}

          {/* Directory Info */}
          {(buildResult?.rule_directory || testResult?.rule_directory) && (
            <div style={{ marginTop: '16px', fontSize: '13px', color: '#666' }}>
              <strong>📂 Directory:</strong> {buildResult?.rule_directory || testResult?.rule_directory}
            </div>
          )}
        </div>
      ),
      okText: 'Done',
    });
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

  // Handle save
  const handleSave = async () => {
    try {
      const values = await form.validateFields();
      
      if (!editorContent.trim()) {
        message.error('Rule content cannot be empty');
        return;
      }

      setLoading(true);

      // Get the name - from form for Actions/ActionSets, parsed from content for Rules
      let ruleName;
      if (rule?.item_type === 'action' || rule?.item_type === 'actionset') {
        ruleName = values.name;
        if (!ruleName) {
          message.error('Please enter a name');
          return;
        }
      } else {
        // Validate that rule name can be parsed for regular rules
        ruleName = parsedRuleName;
        if (!ruleName) {
          message.error('Could not parse rule name from content. Make sure your rule starts with "rule name:"');
          return;
        }
      }

      // Prepare rule data - only send status if user explicitly changed it
      const ruleData = {
        description: values.description,
        name: ruleName,
        content: editorContent,
        schema_version: selectedSchema,
        process_area_id: values.process_area_id,
        context_id: values.context_id || null,
      };

      console.log('Saving rule with context_id:', values.context_id);
      console.log('Form values:', values);

      // Include item_type for Actions and ActionSets
      if (rule?.item_type) {
        ruleData.item_type = rule.item_type;
      }

      // Only include status if it was explicitly set by user (different from initial rule status)
      if (rule && values.status !== rule.status) {
        ruleData.status = values.status;
      } else if (!rule && values.status !== 'DRAFT') {
        // For new rules, only include status if it's not the default
        ruleData.status = values.status;
      }

      let savedRule;
      if (rule && rule.id) {
        // Update existing rule (must have valid ID)
        const response = await rulesApi.updateRule(rule.id, ruleData);
        savedRule = response.data;
        console.log('Rule saved, context_id in response:', savedRule.context_id);
        message.success('Rule updated successfully');
      } else {
        // Create new rule (no rule or no valid ID)
        const response = await rulesApi.createRule(ruleData);
        savedRule = response.data;
        console.log('Rule created, context_id in response:', savedRule.context_id);
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

  // Helper function to generate dynamic title based on item type
  const getContentTitle = () => {
    const itemType = rule?.item_type || 'rule';

    switch (itemType) {
      case 'action':
        return 'Action Definition';
      case 'actionset':
        return 'ActionSet Content';
      case 'non_mon_rule':
        return 'Non-Monetary Rule Content';
      case 'mon_rule':
        return 'Monetary Rule Content';
      case 'rule':
      default:
        return 'Rule Content';
    }
  };

  return (
    <div>
      {/* Header */}
      <div style={{ marginBottom: 24, display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: 16 }}>
          <Button
            icon={<ArrowLeftOutlined />}
            onClick={onBack}
            className="action-button action-button-back"
            style={{
              height: '36px',
              minWidth: '120px',
              borderRadius: '6px',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center'
            }}
          >
            Back to Rules
          </Button>
          <h2>{rule ? `Edit Rule: ${parsedRuleName || rule.name}` : 'Create New Rule'}</h2>
        </div>
        <div style={{ display: 'flex', gap: '24px', alignItems: 'center' }}>
          {/* Main Workflow Buttons */}
          <Space size="large">
            <Button
              type="primary"
              icon={<SaveOutlined />}
              onClick={handleSave}
              loading={loading}
              className="action-button action-button-save"
              style={{
                height: '36px',
                minWidth: '120px',
                backgroundColor: '#1890ff',
                borderColor: '#1890ff',
                color: 'white',
                fontWeight: 500,
                borderRadius: '6px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center'
              }}
            >
              {rule && rule.id ? 'Update Rule' : 'Save Rule'}
            </Button>
            <Button
              icon={<CheckCircleOutlined />}
              onClick={handleValidate}
              loading={validating}
              className="action-button action-button-validate"
              style={{
                height: '36px',
                minWidth: '120px',
                backgroundColor: '#f6ffed',
                borderColor: '#52c41a',
                color: '#52c41a',
                fontWeight: 500,
                borderRadius: '6px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center'
              }}
              onMouseEnter={(e) => {
                e.target.style.backgroundColor = '#52c41a';
                e.target.style.color = 'white';
              }}
              onMouseLeave={(e) => {
                e.target.style.backgroundColor = '#f6ffed';
                e.target.style.color = '#52c41a';
              }}
            >
              Validate
            </Button>
            <Button
              icon={<CloudDownloadOutlined />}
              onClick={handleGenerateProductionCode}
              disabled={!rule || !isExecutableStatus(rule.status)}
              loading={loading}
              className="action-button action-button-generate"
              style={{
                height: '36px',
                minWidth: '180px',
                backgroundColor: rule && isExecutableStatus(rule.status) ? '#f5f7fa' : '#f5f5f5',
                borderColor: rule && isExecutableStatus(rule.status) ? '#4a90b8' : '#d9d9d9',
                color: rule && isExecutableStatus(rule.status) ? '#4a90b8' : '#8c8c8c',
                fontWeight: 500,
                borderRadius: '6px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                opacity: rule && isExecutableStatus(rule.status) ? 1 : 0.6
              }}
              onMouseEnter={(e) => {
                if (rule && isExecutableStatus(rule.status)) {
                  e.target.style.backgroundColor = '#4a90b8';
                  e.target.style.color = 'white';
                }
              }}
              onMouseLeave={(e) => {
                if (rule && isExecutableStatus(rule.status)) {
                  e.target.style.backgroundColor = '#f5f7fa';
                  e.target.style.color = '#4a90b8';
                }
              }}
            >
              Generate Code {(!rule || !isExecutableStatus(rule.status)) && '(VALID+ Only)'}
            </Button>
            <Button
              icon={<ToolOutlined />}
              onClick={handleTestCode}
              disabled={!rule || !isExecutableStatus(rule.status)}
              loading={loading}
              className="action-button action-button-test"
              style={{
                height: '36px',
                minWidth: '160px',
                backgroundColor: rule && isExecutableStatus(rule.status) ? '#fff7e6' : '#f5f5f5',
                borderColor: rule && isExecutableStatus(rule.status) ? '#fa8c16' : '#d9d9d9',
                color: rule && isExecutableStatus(rule.status) ? '#fa8c16' : '#8c8c8c',
                fontWeight: 500,
                borderRadius: '6px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                opacity: rule && isExecutableStatus(rule.status) ? 1 : 0.6
              }}
              onMouseEnter={(e) => {
                if (rule && isExecutableStatus(rule.status)) {
                  e.target.style.backgroundColor = '#fa8c16';
                  e.target.style.color = 'white';
                }
              }}
              onMouseLeave={(e) => {
                if (rule && isExecutableStatus(rule.status)) {
                  e.target.style.backgroundColor = '#fff7e6';
                  e.target.style.color = '#fa8c16';
                }
              }}
            >
              Test Code {(!rule || !isExecutableStatus(rule.status)) && '(VALID+ Only)'}
            </Button>
          </Space>

          {/* Utility Buttons */}
          <Space>
            <Button
              icon={<InfoCircleOutlined />}
              onClick={() => setSchemaViewerVisible(true)}
              className="action-button action-button-schema"
              style={{
                height: '36px',
                minWidth: '140px',
                backgroundColor: '#f8f6fa',
                borderColor: '#8b5d99',
                color: '#8b5d99',
                fontWeight: 500,
                borderRadius: '6px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center'
              }}
              onMouseEnter={(e) => {
                e.target.style.backgroundColor = '#8b5d99';
                e.target.style.color = 'white';
              }}
              onMouseLeave={(e) => {
                e.target.style.backgroundColor = '#f8f6fa';
                e.target.style.color = '#8b5d99';
              }}
            >
              Schema Reference
            </Button>
          </Space>
        </div>
      </div>

      {/* Three Column Layout: Rule Info | Editor | Context Panel */}
      <Row gutter={16}>
        {/* Rule Information */}
        <Col span={contextPanelVisible ? 5 : 8}>
          <Card title="Rule Information" size="small">
            <Form form={form} layout="vertical">
              {rule?.item_type === 'action' || rule?.item_type === 'actionset' ? (
                <Form.Item
                  name="name"
                  label={rule?.item_type === 'action' ? 'Action Name' : 'ActionSet Name'}
                  rules={[{ required: true, message: 'Please enter a name' }]}
                  help="Enter a unique identifier for this item"
                >
                  <Input
                    placeholder={`Enter ${rule?.item_type === 'action' ? 'action' : 'actionset'} name`}
                  />
                </Form.Item>
              ) : (
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
              )}

              <Form.Item name="description" label="Description">
                <TextArea
                  rows={3}
                  placeholder="Enter rule description"
                />
              </Form.Item>

              <Form.Item
                name="process_area_id"
                label="Process Area"
                rules={[{ required: true, message: 'Please select a process area' }]}
              >
                <Select
                  placeholder="Select a process area"
                  loading={loadingProcessAreas}
                  showSearch
                  filterOption={(input, option) =>
                    option.children.toLowerCase().indexOf(input.toLowerCase()) >= 0
                  }
                >
                  {processAreas.map(area => (
                    <Option key={area.id} value={area.id}>
                      {area.name}
                    </Option>
                  ))}
                </Select>
              </Form.Item>

              <Form.Item
                name="context_id"
                label={
                  <Space>
                    <span>Test Context</span>
                    <Button
                      type="link"
                      size="small"
                      onClick={() => setContextManagerVisible(true)}
                      style={{ padding: 0, height: 'auto' }}
                    >
                      Manage Contexts
                    </Button>
                  </Space>
                }
                help="Select a context to use for rule testing and validation"
              >
                <Select
                  placeholder="Select a test context (optional)"
                  loading={loadingContexts}
                  showSearch
                  allowClear
                  onChange={(value) => {
                    console.log('Context selection changed:', value);
                    // Auto-show panel when context selected, hide when cleared
                    if (value && !contextPanelVisible) {
                      setContextPanelVisible(true);
                    } else if (!value && contextPanelVisible) {
                      setContextPanelVisible(false);
                    }
                  }}
                  filterOption={(input, option) =>
                    option.children.toLowerCase().indexOf(input.toLowerCase()) >= 0
                  }
                >
                  {contexts
                    .filter(ctx => !ctx.is_schema_template)
                    .map(context => (
                      <Option key={context.id} value={context.id}>
                        {context.name}
                      </Option>
                    ))}
                </Select>
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

            {/* Panel Mode Switching */}
            <Divider />
            <div style={{ display: 'flex', alignItems: 'center', gap: 8, marginBottom: 16 }}>
              <Button
                size="small"
                type={panelMode === 'validation' ? 'primary' : 'default'}
                onClick={() => setPanelMode('validation')}
              >
                Validation
              </Button>
              <Button
                size="small"
                type={panelMode === 'context' ? 'primary' : 'default'}
                onClick={() => setPanelMode('context')}
                disabled={!contextInfo}
              >
                Context Help
              </Button>
            </div>

            {/* Validation Panel */}
            {panelMode === 'validation' && validation && (
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
                  // Only show this alert if there are non-missing-item errors
                  validation.errors && validation.errors.some(e => {
                    const errStr = typeof e === 'string' ? e : e.message || JSON.stringify(e);
                    return !errStr.includes('Unknown action') && !errStr.includes('Unknown attribute');
                  }) && (
                    <Alert
                      message="Validation Error"
                      description={
                        <div>
                          <div>{validation.message}</div>
                          {validation.errors && validation.errors.length > 0 && (
                            <ul style={{ marginTop: 8, marginBottom: 0 }}>
                              {validation.errors
                                .filter(error => {
                                  const errStr = typeof error === 'string' ? error : error.message || JSON.stringify(error);
                                  return !errStr.includes('Unknown action') && !errStr.includes('Unknown attribute');
                                })
                                .map((error, index) => (
                                  <li key={index}>{typeof error === 'string' ? error : error.message || JSON.stringify(error)}</li>
                                ))}
                            </ul>
                          )}
                        </div>
                      }
                      type="error"
                      showIcon
                      icon={<CloseCircleOutlined />}
                    />
                  )
                )}
                {/* Enhanced validation display with statistics cards */}
                {validation && (validation.warnings?.length > 0 || validation.undefined_attributes?.length > 0 || validation.undefined_actions?.length > 0) && (
                  <>
                    {/* Statistics cards for missing items (matches Gap Analysis UX) */}
                    {(validation.undefined_attributes?.length > 0 || validation.undefined_actions?.length > 0) && (
                      <Row gutter={16} style={{ marginTop: 16 }}>
                        {validation.undefined_attributes?.length > 0 && (
                          <Col span={12}>
                            <Card size="small" style={{ borderColor: '#ff4d4f' }}>
                              <Statistic
                                title="Missing Attributes"
                                value={validation.undefined_attributes.length}
                                valueStyle={{ color: '#cf1322' }}
                                prefix={<WarningOutlined />}
                              />
                              <div style={{ marginTop: 8, fontSize: 12, color: '#595959' }}>
                                {validation.undefined_attributes.join(', ')}
                              </div>
                            </Card>
                          </Col>
                        )}

                        {validation.undefined_actions?.length > 0 && (
                          <Col span={validation.undefined_attributes?.length > 0 ? 12 : 24}>
                            <Card size="small" style={{ borderColor: '#ff4d4f' }}>
                              <Statistic
                                title="Missing Actions/ActionSets"
                                value={validation.undefined_actions.length}
                                valueStyle={{ color: '#cf1322' }}
                                prefix={<WarningOutlined />}
                              />
                              <div style={{ marginTop: 8, fontSize: 12, color: '#595959' }}>
                                {validation.undefined_actions.join(', ')}
                              </div>
                            </Card>
                          </Col>
                        )}
                      </Row>
                    )}

                    {/* Other warnings (non-missing-item warnings) */}
                    {validation.warnings?.some(w =>
                      !w.includes('Unknown attribute') && !w.includes('Unknown action')
                    ) && (
                      <Alert
                        style={{ marginTop: 12 }}
                        message="Other Warnings"
                        description={
                          <ul style={{ marginBottom: 0 }}>
                            {validation.warnings
                              .filter(w => !w.includes('Unknown attribute') && !w.includes('Unknown action'))
                              .map((warning, index) => (
                                <li key={index}>{warning}</li>
                              ))}
                          </ul>
                        }
                        type="warning"
                        showIcon
                      />
                    )}
                  </>
                )}
              </div>
            )}

            {/* Context Help Panel */}
            {panelMode === 'context' && (
              <div>
                <h4>Context Information</h4>
                {loadingContext ? (
                  <div style={{ textAlign: 'center', padding: 20 }}>
                    <Spin size="small" />
                    <span style={{ marginLeft: 8 }}>Loading context...</span>
                  </div>
                ) : contextInfo ? (
                  <div>
                    {contextInfo.type === 'error' ? (
                      <Alert
                        message="Error Loading Context"
                        description={contextInfo.message}
                        type="error"
                        showIcon
                      />
                    ) : contextInfo.item_type === 'action' ? (
                      // Action Context
                      <div>
                        <div style={{ marginBottom: 16 }}>
                          <Tag color="blue">Action</Tag>
                          <strong style={{ marginLeft: 8 }}>{contextInfo.name}</strong>
                        </div>

                        {contextInfo.description && (
                          <div style={{ marginBottom: 12 }}>
                            <strong>Description:</strong>
                            <div style={{ marginTop: 4, padding: 8, backgroundColor: '#f5f5f5', borderRadius: 4 }}>
                              {contextInfo.description}
                            </div>
                          </div>
                        )}

                        {contextInfo.javaSource ? (
                          <div>
                            <strong>Java Implementation:</strong>
                            <div style={{
                              marginTop: 8,
                              height: 400,
                              border: '1px solid #d9d9d9',
                              borderRadius: 6
                            }}>
                              <Editor
                                height="100%"
                                language="java"
                                value={contextInfo.javaSource.source_code}
                                options={{
                                  readOnly: true,
                                  minimap: { enabled: false },
                                  fontSize: 12,
                                  lineNumbers: 'on',
                                  roundedSelection: false,
                                  scrollBeyondLastLine: false,
                                  automaticLayout: true,
                                  tabSize: 4,
                                  wordWrap: 'on',
                                  theme: 'vs'
                                }}
                              />
                            </div>
                            <div style={{ marginTop: 8, fontSize: 12, color: '#666' }}>
                              File: {contextInfo.javaSource.java_file_path} ({contextInfo.javaSource.file_size} characters)
                            </div>
                          </div>
                        ) : (
                          <Alert
                            message="No Java Implementation Available"
                            description="This action does not have an associated Java implementation file."
                            type="warning"
                            showIcon
                            size="small"
                          />
                        )}
                      </div>
                    ) : contextInfo.item_type === 'actionset' ? (
                      // ActionSet Context
                      <div>
                        <div style={{ marginBottom: 16 }}>
                          <Tag color="green">ActionSet</Tag>
                          <strong style={{ marginLeft: 8 }}>{contextInfo.name}</strong>
                        </div>

                        {contextInfo.description && (
                          <div style={{ marginBottom: 12 }}>
                            <strong>Description:</strong>
                            <div style={{ marginTop: 4, padding: 8, backgroundColor: '#f5f5f5', borderRadius: 4 }}>
                              {contextInfo.description}
                            </div>
                          </div>
                        )}

                        <div>
                          <strong>Rule Definition:</strong>
                          <div style={{
                            marginTop: 8,
                            maxHeight: 300,
                            overflow: 'auto',
                            backgroundColor: '#fafafa',
                            border: '1px solid #d9d9d9',
                            padding: 12,
                            borderRadius: 6,
                            fontSize: '13px',
                            fontFamily: 'monospace',
                            lineHeight: 1.4
                          }}>
                            <pre style={{ margin: 0 }}>
                              {contextInfo.content}
                            </pre>
                          </div>
                        </div>
                      </div>
                    ) : contextInfo.item_type === 'attribute' ? (
                      // Attribute Schema Context
                      <div>
                        <div style={{ marginBottom: 16 }}>
                          <Tag color="orange">Attribute</Tag>
                          <strong style={{ marginLeft: 8 }}>
                            {contextInfo.qualified_name || contextInfo.name}
                          </strong>
                        </div>

                        {/* Entity Information */}
                        {contextInfo.entity_name && (
                          <div style={{ marginBottom: 12, padding: 8, backgroundColor: '#f0f8ff', borderRadius: 4, border: '1px solid #d9e6f2' }}>
                            <div style={{ fontSize: 12, color: '#1890ff', marginBottom: 4 }}>
                              <strong>📁 Entity:</strong> {contextInfo.entity_name}
                            </div>
                            {contextInfo.entity_description && (
                              <div style={{ fontSize: 11, color: '#666' }}>
                                {contextInfo.entity_description}
                              </div>
                            )}
                          </div>
                        )}

                        <div style={{ display: 'flex', flexDirection: 'column', gap: 8 }}>
                          <div>
                            <strong>Data Type:</strong>
                            <Tag color="purple" style={{ marginLeft: 8 }}>
                              {contextInfo.data_type || 'Unknown'}
                            </Tag>
                          </div>

                          {contextInfo.description && (
                            <div>
                              <strong>Description:</strong>
                              <div style={{ marginTop: 4, padding: 8, backgroundColor: '#f5f5f5', borderRadius: 4 }}>
                                {contextInfo.description}
                              </div>
                            </div>
                          )}

                          {contextInfo.is_required !== undefined && (
                            <div>
                              <strong>Required:</strong>
                              <Tag color={contextInfo.is_required ? 'red' : 'default'} style={{ marginLeft: 8 }}>
                                {contextInfo.is_required ? 'Yes' : 'No'}
                              </Tag>
                            </div>
                          )}

                          {contextInfo.default_value && (
                            <div>
                              <strong>Default Value:</strong>
                              <code style={{ marginLeft: 8, padding: '2px 6px', backgroundColor: '#f5f5f5', borderRadius: 3 }}>
                                {contextInfo.default_value}
                              </code>
                            </div>
                          )}

                          {contextInfo.validation_rules && (
                            <div>
                              <strong>Validation Rules:</strong>
                              <div style={{ marginTop: 4, padding: 8, backgroundColor: '#f5f5f5', borderRadius: 4 }}>
                                {contextInfo.validation_rules}
                              </div>
                            </div>
                          )}

                          {(contextInfo.min_value || contextInfo.max_value) && (
                            <div>
                              <strong>Range:</strong>
                              <span style={{ marginLeft: 8 }}>
                                {contextInfo.min_value && `Min: ${contextInfo.min_value}`}
                                {contextInfo.min_value && contextInfo.max_value && ', '}
                                {contextInfo.max_value && `Max: ${contextInfo.max_value}`}
                              </span>
                            </div>
                          )}
                        </div>
                      </div>
                    ) : (
                      // Unknown context type
                      <Alert
                        message={contextInfo.message || 'Unknown Context Type'}
                        description={contextInfo.name ? `Word: "${contextInfo.name}"` : undefined}
                        type="info"
                        showIcon
                      />
                    )}
                  </div>
                ) : (
                  <Alert
                    message="No Context Information"
                    description="Right-click on actions, actionsets, or attributes in the editor to see context information here."
                    type="info"
                    showIcon
                  />
                )}
              </div>
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
        <Col span={contextPanelVisible ? 13 : 16}>
          <Card title={getContentTitle()} size="small">
            <div style={{ height: 600, border: '1px solid #d9d9d9', borderRadius: 6 }}>
              <Editor
                height="calc(100% - 24px)"
                language={rule?.item_type === 'action' ? 'json' : 'rules'}
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

        {/* Context Panel */}
        {contextPanelVisible && (
          <Col span={6}>
            <ContextPanel
              contextId={form.getFieldValue('context_id')}
              onClose={() => setContextPanelVisible(false)}
            />
          </Col>
        )}
      </Row>

      {/* Schema Viewer Modal */}
      <SchemaViewer
        visible={schemaViewerVisible}
        onClose={() => setSchemaViewerVisible(false)}
        schemaVersion={selectedSchema}
        contextId={form.getFieldValue('context_id')}
      />

      {/* Context Manager Modal */}
      <ContextManager
        visible={contextManagerVisible}
        onClose={() => setContextManagerVisible(false)}
        onContextCreated={() => {
          loadContexts();
          message.success('Context created! Select it from the dropdown above.');
        }}
      />

    </div>
  );
};

export default RuleEditor;