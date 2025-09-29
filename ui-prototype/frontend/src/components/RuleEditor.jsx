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
  CheckCircleOutlined,
  CloseCircleOutlined,
  ArrowLeftOutlined,
  CloudDownloadOutlined,
  ToolOutlined,
  InfoCircleOutlined,
  DatabaseOutlined,
  SaveOutlined,
} from '@ant-design/icons';
import Editor from '@monaco-editor/react';
import { rulesApi } from '../services/api';
import { rulesLanguageDefinition, getMonacoCompletionKind } from '../utils/rulesSyntax';
import suggestionCache from '../services/suggestionCache';
import SchemaViewer from './SchemaViewer';
import SampleDataEditor from './SampleDataEditor';
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
  const [sampleDataVisible, setSampleDataVisible] = useState(false);
  const [selectedSchema, setSelectedSchema] = useState('modern');
  const [parsedRuleName, setParsedRuleName] = useState('');
  const [cursorPosition, setCursorPosition] = useState({ line: 1, column: 1 });
  const [processAreas, setProcessAreas] = useState([]);
  const [loadingProcessAreas, setLoadingProcessAreas] = useState(false);
  const [panelMode, setPanelMode] = useState('validation'); // 'validation' or 'context'
  const [contextInfo, setContextInfo] = useState(null);
  const [loadingContext, setLoadingContext] = useState(false);
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

  // Initialize form and editor
  useEffect(() => {
    if (rule) {
      form.setFieldsValue({
        description: rule.description,
        status: rule.status,
        process_area_id: rule.process_area_id,
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

  // Load process areas on component mount
  useEffect(() => {
    loadProcessAreas();
  }, []);

  // Load context information for a word
  const loadContextInfo = async (word, position, editorContent) => {
    try {
      console.log('🎯 loadContextInfo called with word:', word);
      setLoadingContext(true);
      setPanelMode('context');

      // Determine context type using the context API service
      const contextType = await determineContextType(word, position, editorContent);
      console.log('🔎 contextType determined:', contextType);

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

    console.log('🔍 extractContextWord DEBUG:', { lineContent, column });

    // Check if we're in a quoted string first
    const quotedString = extractQuotedString(lineContent, column);
    if (quotedString) {
      console.log('✅ Found quoted string:', quotedString);
      return quotedString;
    }

    // Always try our compound identifier extraction first (handles camelCase, snake_case, dot notation)
    const compoundIdentifier = extractCompoundIdentifier(lineContent, column);
    if (compoundIdentifier) {
      console.log('✅ Found compound identifier:', compoundIdentifier);
      return compoundIdentifier;
    }

    // Only fall back to Monaco if our extraction found nothing
    const word = model.getWordAtPosition(position);
    const monacoWord = word ? word.word : null;
    console.log('⚠️ Falling back to Monaco word:', monacoWord);
    return monacoWord;
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

  // Handle sample data
  const handleSampleData = () => {
    setSampleDataVisible(true);
  };

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

  // Enhanced test rule with validation-first workflow
  const handleTest = async () => {
    try {
      if (!editorContent.trim()) {
        message.error('Please enter rule content before testing');
        return;
      }

      setLoading(true);
      const testSteps = [];
      let totalStartTime = performance.now();

      // STEP 1: Validation (Fast - fail fast)
      message.loading('Step 1/4: Validating rule syntax...', 0);
      const validationStart = performance.now();

      let validationResult;
      try {
        const validationResponse = await rulesApi.validateRule(editorContent);
        validationResult = validationResponse.data;
        testSteps.push({
          step: 'validation',
          duration: Math.round(performance.now() - validationStart),
          success: validationResult.valid,
          data: validationResult
        });
      } catch (error) {
        message.error('Validation failed: ' + error.message);
        return;
      }

      // Stop if validation fails
      if (!validationResult.valid) {
        setLoading(false);
        showEnhancedTestResults({
          success: false,
          message: 'Rule validation failed',
          steps: testSteps,
          validationErrors: validationResult.errors || [validationResult.message],
          totalTimeMs: Math.round(performance.now() - totalStartTime)
        });
        return;
      }

      // Rules can call ActionSets directly by name - continue with full pipeline

      // STEP 2: Code Generation (Optional preview)
      message.loading('Step 2/4: Generating Java code...', 0);
      const codeGenStart = performance.now();

      let generatedCode = null;
      try {
        const codeResponse = await rulesApi.generateCode(editorContent);
        const codeResult = codeResponse.data;
        generatedCode = codeResult.javaCode;
        testSteps.push({
          step: 'codeGeneration',
          duration: Math.round(performance.now() - codeGenStart),
          success: codeResult.success,
          data: { codeLength: generatedCode?.length || 0 }
        });
      } catch (error) {
        testSteps.push({
          step: 'codeGeneration',
          duration: Math.round(performance.now() - codeGenStart),
          success: false,
          error: error.message
        });
      }

      // STEP 3 & 4: Test rule using consolidated Python backend
      message.loading('Step 3/4: Testing rule with Python backend...', 0);
      const testStart = performance.now();

      // Get test data
      const testData = getTestData();

      let testResult;
      try {
        const testResponse = await fetch('/api/rules/test', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            rule_content: editorContent,
            test_data: testData
          })
        });

        if (!testResponse.ok) {
          throw new Error(`HTTP ${testResponse.status}: ${testResponse.statusText}`);
        }

        testResult = await testResponse.json();

        testSteps.push({
          step: 'test',
          duration: Math.round(performance.now() - testStart),
          success: testResult.success,
          data: {
            testTimeMs: testResult.performance?.testTimeMs || 0,
            compilationTimeMs: testResult.performance?.compilationTimeMs || 0,
            executionTimeMs: testResult.performance?.executionTimeMs || 0,
            result: testResult.result
          },
          error: testResult.success ? null : (testResult.message || JSON.stringify(testResult))
        });
      } catch (error) {
        setLoading(false);
        message.destroy();
        message.error('Rule testing failed: ' + error.message);
        return;
      }

      if (!testResult.success) {
        setLoading(false);
        message.destroy();
        showEnhancedTestResults({
          success: false,
          message: 'Rule testing failed',
          steps: testSteps,
          compilationError: testResult.message,
          totalTimeMs: Math.round(performance.now() - totalStartTime)
        });
        return;
      }

      // Skip the separate execution step since it's now combined
      message.loading('Step 4/4: Finalizing results...', 0);

      let executionResult = testResult;

      // Clear all loading messages and show results
      message.destroy();
      const totalTime = Math.round(performance.now() - totalStartTime);
      message.success(`Test completed in ${totalTime}ms`, 2);

      showEnhancedTestResults({
        success: true,
        message: 'Test completed successfully',
        steps: testSteps,
        executionResult: executionResult,
        generatedCode: generatedCode,
        totalTimeMs: totalTime,
        ruleName: parsedRuleName
      });

    } catch (error) {
      message.error('Test failed: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  // Helper function to get test data
  const getTestData = () => {
    const savedSampleData = localStorage.getItem('sampleTestData');
    if (savedSampleData) {
      const parsedSampleData = JSON.parse(savedSampleData);
      return {
        applicant: JSON.parse(parsedSampleData.applicant),
        transaction: JSON.parse(parsedSampleData.transaction),
        account: JSON.parse(parsedSampleData.account),
        metadata: {
          business_date: new Date().toISOString().split('T')[0],
          test_timestamp: new Date().toISOString()
        }
      };
    }

    // Default test data
    return {
      applicant: {
        creditScore: 750,
        age: 28,
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
            COMPILED
          </Tag>
          {performance.method && (
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
          {performance.method && (
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

  // Show enhanced test results with step-by-step breakdown
  const showEnhancedTestResults = (result) => {
    const {
      success,
      message: resultMessage,
      steps = [],
      validationErrors = [],
      compilationError = '',
      executionResult = {},
      generatedCode = '',
      totalTimeMs = 0,
      ruleName = ''
    } = result;

    Modal.info({
      title: (
        <div style={{ display: 'flex', alignItems: 'center', gap: '12px' }}>
          <span>🧪 Enhanced Rule Test Results</span>
          <Tag color={success ? 'green' : 'red'}>
            {success ? 'SUCCESS' : 'FAILED'}
          </Tag>
          <Tag color="blue" style={{ fontSize: '10px' }}>
            {totalTimeMs}ms total
          </Tag>
        </div>
      ),
      width: 1000,
      content: (
        <div>
          {/* Executive Summary */}
          <Card
            style={{
              marginBottom: '20px',
              background: success ? '#f6ffed' : '#fff2f0',
              border: `1px solid ${success ? '#b7eb8f' : '#ffccc7'}`
            }}
          >
            <div style={{ textAlign: 'center', padding: '8px 0' }}>
              <div style={{ fontSize: '16px', fontWeight: 'bold', marginBottom: '8px' }}>
                {resultMessage}
              </div>
              <div style={{ fontSize: '14px', color: '#666' }}>
                Rule: <strong>{ruleName || 'Test Rule'}</strong> |
                Total Time: {totalTimeMs}ms
              </div>
            </div>
          </Card>

          {/* Step-by-Step Breakdown */}
          <div style={{ marginBottom: '20px' }}>
            <h4 style={{ marginBottom: '12px' }}>📊 Execution Pipeline</h4>
            <div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
              {steps.map((step, index) => {
                const stepNames = {
                  validation: '1️⃣ Syntax Validation',
                  codeGeneration: '2️⃣ Code Generation',
                  compilation: '3️⃣ Compilation',
                  execution: '4️⃣ Execution'
                };

                return (
                  <Card
                    key={index}
                    size="small"
                    style={{
                      border: `1px solid ${step.success ? '#52c41a' : '#ff7875'}`,
                      background: step.success ? '#f6ffed' : '#fff2f0'
                    }}
                  >
                    <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                      <div style={{ display: 'flex', alignItems: 'center', gap: '12px' }}>
                        <Tag
                          color={step.success ? 'green' : 'red'}
                          style={{ fontSize: '12px', minWidth: '60px' }}
                        >
                          {step.success ? '✓ PASS' : '✗ FAIL'}
                        </Tag>
                        <span style={{ fontWeight: '500' }}>
                          {stepNames[step.step] || step.step}
                        </span>
                        {step.error && (
                          <span style={{ color: '#ff4d4f', fontSize: '12px' }}>
                            - {step.error}
                          </span>
                        )}
                      </div>
                      <Tag color="blue" size="small">
                        {step.duration}ms
                      </Tag>
                    </div>
                  </Card>
                );
              })}
            </div>
          </div>

          {/* Validation Errors */}
          {validationErrors.length > 0 && (
            <div style={{ marginBottom: '20px' }}>
              <Card style={{ background: '#fff2f0', border: '1px solid #ffccc7' }}>
                <h4 style={{ color: '#cf1322', marginBottom: '12px' }}>
                  ❌ Syntax Errors Found
                </h4>
                <div style={{ fontSize: '12px' }}>
                  {validationErrors.map((error, index) => (
                    <div
                      key={index}
                      style={{
                        padding: '8px 12px',
                        background: '#ffffff',
                        border: '1px solid #ffccc7',
                        borderRadius: '4px',
                        marginBottom: '8px',
                        fontFamily: 'monospace'
                      }}
                    >
                      {typeof error === 'string' ? error : error.message || JSON.stringify(error)}
                    </div>
                  ))}
                </div>
                <div style={{ marginTop: '12px', padding: '8px', background: '#fff7e6', borderRadius: '4px' }}>
                  <strong>💡 Quick Fixes:</strong>
                  <ul style={{ marginTop: '4px', marginBottom: '0', fontSize: '12px' }}>
                    <li>Use <code>=</code> for equality, not <code>==</code></li>
                    <li>Use <code>and</code>/<code>or</code> instead of <code>&&</code>/<code>||</code></li>
                    <li>Ensure proper rule syntax: <code>rule name: if condition then action</code></li>
                  </ul>
                </div>
              </Card>
            </div>
          )}

          {/* Compilation Error */}
          {compilationError && (
            <div style={{ marginBottom: '20px' }}>
              <Card style={{ background: '#fff2f0', border: '1px solid #ffccc7' }}>
                <h4 style={{ color: '#cf1322', marginBottom: '12px' }}>
                  🔧 Compilation Error
                </h4>
                <div style={{
                  fontFamily: 'monospace',
                  fontSize: '12px',
                  padding: '12px',
                  background: '#ffffff',
                  border: '1px solid #ffccc7',
                  borderRadius: '4px'
                }}>
                  {compilationError}
                </div>
              </Card>
            </div>
          )}

          {/* Execution Results */}
          {success && executionResult.success && (
            <div style={{ marginBottom: '20px' }}>
              <Card style={{ background: '#f6ffed', border: '1px solid #b7eb8f' }}>
                <h4 style={{ color: '#389e0d', marginBottom: '12px' }}>
                  ⚡ Execution Results
                </h4>
                <div style={{ marginBottom: '12px' }}>
                  <div style={{ fontSize: '14px', marginBottom: '8px' }}>
                    <strong>Rule Matched:</strong> {executionResult.matched ? 'Yes' : 'No'} |
                    <strong> Actions:</strong> {executionResult.actions?.length || 0} |
                    <strong> Execution Time:</strong> {executionResult.executionTimeMs || '<1'}ms
                  </div>
                  {executionResult.actions && executionResult.actions.length > 0 && (
                    <div>
                      <strong>Executed Actions:</strong>
                      <div style={{ marginTop: '4px' }}>
                        {executionResult.actions.map((action, index) => (
                          <Tag key={index} color="green" style={{ margin: '2px' }}>
                            {action}
                          </Tag>
                        ))}
                      </div>
                    </div>
                  )}
                </div>
              </Card>
            </div>
          )}

          {/* Generated Code Preview (Collapsible) */}
          {generatedCode && (
            <div style={{ marginBottom: '20px' }}>
              <details>
                <summary style={{
                  cursor: 'pointer',
                  fontSize: '14px',
                  fontWeight: 'bold',
                  color: '#1890ff',
                  padding: '8px 0',
                  borderBottom: '1px solid #f0f0f0'
                }}>
                  📄 View Generated Java Code ({generatedCode.length} characters)
                </summary>
                <div style={{
                  marginTop: '12px',
                  maxHeight: '300px',
                  overflow: 'auto',
                  background: '#fafafa',
                  border: '1px solid #d9d9d9',
                  borderRadius: '4px'
                }}>
                  <pre style={{
                    margin: 0,
                    padding: '16px',
                    fontSize: '12px',
                    fontFamily: 'monospace',
                    lineHeight: '1.4'
                  }}>
                    {generatedCode}
                  </pre>
                </div>
              </details>
            </div>
          )}

          <div style={{
            fontSize: '11px',
            color: '#999',
            textAlign: 'center',
            marginTop: '16px',
            paddingTop: '12px',
            borderTop: '1px solid #f0f0f0'
          }}>
            Enhanced test completed at: {new Date().toLocaleString()}
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
        process_area_id: values.process_area_id,
      };

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
        message.success('Rule updated successfully');
      } else {
        // Create new rule (no rule or no valid ID)
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

  // Helper function to generate dynamic title based on item type
  const getContentTitle = () => {
    const itemType = rule?.item_type || 'rule';

    switch (itemType) {
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
              height: '40px',
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
                height: '40px',
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
                height: '40px',
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
                height: '40px',
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
                height: '40px',
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
                height: '40px',
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
            <Button
              icon={<DatabaseOutlined />}
              onClick={handleSampleData}
              className="action-button action-button-sample"
              style={{
                height: '40px',
                minWidth: '120px',
                backgroundColor: '#faf7f2',
                borderColor: '#a67c5a',
                color: '#8b6f47',
                fontWeight: 500,
                borderRadius: '6px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center'
              }}
              onMouseEnter={(e) => {
                e.target.style.backgroundColor = '#a67c5a';
                e.target.style.color = 'white';
              }}
              onMouseLeave={(e) => {
                e.target.style.backgroundColor = '#faf7f2';
                e.target.style.color = '#8b6f47';
              }}
            >
              Sample Data
            </Button>
          </Space>
        </div>
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
                  <Alert
                    message={validation.message && validation.message.toLowerCase().includes('valid') ? 'Validation Warning' : 'Invalid'}
                    description={
                      <div>
                        <div>{validation.message}</div>
                        {validation.errors && validation.errors.length > 0 && (
                          <ul style={{ marginTop: 8, marginBottom: 0 }}>
                            {validation.errors.map((error, index) => (
                              <li key={index}>{typeof error === 'string' ? error : error.message || JSON.stringify(error)}</li>
                            ))}
                          </ul>
                        )}
                      </div>
                    }
                    type={validation.message && validation.message.toLowerCase().includes('valid') ? 'warning' : 'error'}
                    showIcon
                    icon={validation.message && validation.message.toLowerCase().includes('valid') ? <InfoCircleOutlined /> : <CloseCircleOutlined />}
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
        <Col span={16}>
          <Card title={getContentTitle()} size="small">
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