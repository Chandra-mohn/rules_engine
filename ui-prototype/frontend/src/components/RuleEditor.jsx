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
} from 'antd';
import {
  SaveOutlined,
  PlayCircleOutlined,
  CheckCircleOutlined,
  CloseCircleOutlined,
  ArrowLeftOutlined,
  InfoCircleOutlined,
} from '@ant-design/icons';
import Editor from '@monaco-editor/react';
import { rulesApi } from '../services/api';
import { rulesLanguageDefinition } from '../utils/rulesSyntax';
import SchemaViewer from './SchemaViewer';

const { TextArea } = Input;
const { Option } = Select;

const RuleEditor = ({ rule, onBack, onSave }) => {
  const [form] = Form.useForm();
  const [loading, setLoading] = useState(false);
  const [validating, setValidating] = useState(false);
  const [validation, setValidation] = useState(null);
  const [editorContent, setEditorContent] = useState('');
  const [schemaViewerVisible, setSchemaViewerVisible] = useState(false);
  const editorRef = useRef(null);
  const monacoRef = useRef(null);

  // Initialize form and editor
  useEffect(() => {
    if (rule) {
      form.setFieldsValue({
        name: rule.name,
        description: rule.description,
        status: rule.status,
      });
      setEditorContent(rule.content);
      setValidation({
        valid: rule.validation_status === 'valid',
        message: rule.validation_message,
      });
    } else {
      // New rule defaults
      form.setFieldsValue({
        status: 'draft',
      });
      setEditorContent('rule newRule:\n    if condition then action');
    }
  }, [rule, form]);

  // Handle Monaco editor mount
  const handleEditorDidMount = (editor, monaco) => {
    editorRef.current = editor;
    monacoRef.current = monaco;

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

          const response = await rulesApi.getAutocompleteSuggestions(
            textUntilPosition,
            textUntilPosition.length
          );

          const suggestions = response.data.suggestions.map(suggestion => ({
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

  // Handle test rule
  const handleTest = () => {
    // For now, show a simple test interface
    // In a full implementation, this would open a comprehensive test modal
    message.info('Not yet implemented');
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

      const ruleData = {
        ...values,
        content: editorContent,
      };

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
          <h2>{rule ? `Edit Rule: ${rule.name}` : 'Create New Rule'}</h2>
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
                name="name"
                label="Rule Name"
                rules={[
                  { required: true, message: 'Rule name is required' },
                  { pattern: /^[a-zA-Z][a-zA-Z0-9_]*$/, message: 'Invalid rule name format' },
                ]}
              >
                <Input placeholder="Enter rule name" />
              </Form.Item>

              <Form.Item name="description" label="Description">
                <TextArea
                  rows={3}
                  placeholder="Enter rule description"
                />
              </Form.Item>

              <Form.Item name="status" label="Status">
                <Select>
                  <Option value="draft">Draft</Option>
                  <Option value="active">Active</Option>
                  <Option value="inactive">Inactive</Option>
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
                height="100%"
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
      />
    </div>
  );
};

export default RuleEditor;