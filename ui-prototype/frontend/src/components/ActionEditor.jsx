import React, { useState, useEffect } from 'react';
import {
  Form,
  Input,
  Button,
  Select,
  Card,
  message,
  Alert,
  Modal,
  List,
  Typography,
  Space,
  Spin,
} from 'antd';
import {
  ArrowLeftOutlined,
  FolderOpenOutlined,
  CheckCircleOutlined,
  WarningOutlined,
  FileOutlined,
  CodeOutlined,
} from '@ant-design/icons';
import Editor from '@monaco-editor/react';
import axios from 'axios';

const { TextArea } = Input;
const { Option } = Select;
const { Text } = Typography;

const ActionEditor = ({ action, onBack, onSave }) => {
  const [form] = Form.useForm();
  const [loading, setLoading] = useState(false);
  const [javaFilePath, setJavaFilePath] = useState('');
  const [javaContent, setJavaContent] = useState('');
  const [fileExists, setFileExists] = useState(null);
  const [loadingFile, setLoadingFile] = useState(false);
  const [processAreas, setProcessAreas] = useState([]);
  const [loadingProcessAreas, setLoadingProcessAreas] = useState(false);
  const [javaFiles, setJavaFiles] = useState([]);
  const [showFilePicker, setShowFilePicker] = useState(false);

  // Load process areas
  useEffect(() => {
    loadProcessAreas();
  }, []);

  // Initialize form
  useEffect(() => {
    if (action) {
      form.setFieldsValue({
        name: action.name,
        description: action.description,
        process_area_id: action.process_area_id,
        status: action.status || 'DRAFT',
      });
      setJavaFilePath(action.content || '');
    } else {
      form.setFieldsValue({ status: 'DRAFT' });
    }
  }, [action, form]);

  // Load Java file preview when path changes
  useEffect(() => {
    if (javaFilePath && javaFilePath.trim()) {
      loadJavaFilePreview(javaFilePath);
    } else {
      setJavaContent('');
      setFileExists(null);
    }
  }, [javaFilePath]);

  const loadProcessAreas = async () => {
    setLoadingProcessAreas(true);
    try {
      const response = await axios.get('/api/hierarchy/process-areas');
      setProcessAreas(response.data.process_areas || []);
    } catch (error) {
      message.error('Failed to load process areas');
      console.error('Process areas load error:', error);
    } finally {
      setLoadingProcessAreas(false);
    }
  };

  const loadJavaFilePreview = async (path) => {
    setLoadingFile(true);
    try {
      const response = await axios.get(`/api/java-files/content?path=${encodeURIComponent(path)}`);

      if (response.status === 200) {
        setJavaContent(response.data.content);
        setFileExists(true);
      }
    } catch (error) {
      if (error.response?.status === 404) {
        setJavaContent(`// File not found: ${path}\n// You can still save this action - the file may be created later.`);
        setFileExists(false);
      } else {
        setJavaContent(`// Error loading file: ${error.message}`);
        setFileExists(false);
      }
    } finally {
      setLoadingFile(false);
    }
  };

  const loadJavaFilesList = async () => {
    try {
      const response = await axios.get('/api/java-files/list');
      setJavaFiles(response.data.files || []);
      return response.data.files || [];
    } catch (error) {
      message.error('Failed to load Java files list');
      return [];
    }
  };

  const handleBrowseFiles = async () => {
    await loadJavaFilesList();
    setShowFilePicker(true);
  };

  const handleFileSelect = (filePath) => {
    setJavaFilePath(filePath);
    setShowFilePicker(false);
    form.setFieldsValue({ javaFilePath: filePath });
  };

  const handleSave = async () => {
    try {
      const values = await form.validateFields();
      setLoading(true);

      if (!javaFilePath || !javaFilePath.trim()) {
        message.error('Please specify a Java file path');
        return;
      }

      const actionData = {
        name: values.name,
        description: values.description,
        content: javaFilePath.trim(), // Just the file path
        process_area_id: values.process_area_id,
        status: values.status,
        item_type: 'action',
      };

      let savedAction;
      if (action?.id) {
        const response = await axios.put(`/api/rules/${action.id}`, actionData);
        savedAction = response.data;
        message.success('Action updated successfully');
      } else {
        const response = await axios.post('/api/rules', actionData);
        savedAction = response.data;
        message.success('Action created successfully');
      }

      if (onSave) {
        onSave(savedAction);
      }
    } catch (error) {
      message.error('Failed to save action: ' + (error.response?.data?.error || error.message));
    } finally {
      setLoading(false);
    }
  };

  return (
    <div>
      {/* Header */}
      <div style={{ marginBottom: 24 }}>
        <Button
          icon={<ArrowLeftOutlined />}
          onClick={onBack}
          style={{
            height: '40px',
            minWidth: '120px',
          }}
        >
          Back to List
        </Button>
      </div>

      <Card title={<><CodeOutlined /> {action ? 'Edit Action' : 'Create New Action'}</>}>
        <Form form={form} layout="vertical">
          <Form.Item
            name="name"
            label="Action Name"
            rules={[
              { required: true, message: 'Please enter action name' },
              { pattern: /^[a-zA-Z_][a-zA-Z0-9_]*$/, message: 'Use only letters, numbers, and underscores' }
            ]}
            help="A unique identifier for this action (e.g., approve, reject, sendEmail)"
          >
            <Input placeholder="e.g., approve" />
          </Form.Item>

          <Form.Item
            name="description"
            label="Description"
            rules={[{ required: true, message: 'Please enter description' }]}
          >
            <TextArea rows={2} placeholder="What does this action do?" />
          </Form.Item>

          <Form.Item
            name="process_area_id"
            label="Process Area"
            rules={[{ required: true, message: 'Please select process area' }]}
          >
            <Select
              placeholder="Select process area"
              loading={loadingProcessAreas}
              showSearch
              filterOption={(input, option) =>
                option.children.toLowerCase().indexOf(input.toLowerCase()) >= 0
              }
            >
              {processAreas.map(area => (
                <Option key={area.id} value={area.id}>{area.name}</Option>
              ))}
            </Select>
          </Form.Item>

          <Form.Item name="status" label="Status">
            <Select>
              <Option value="DRAFT">Draft</Option>
              <Option value="VALID">Valid</Option>
              <Option value="PROD">Production</Option>
            </Select>
          </Form.Item>

          <Form.Item label="Java File" required help="Path to the Java file implementing this action">
            <Space.Compact style={{ width: '100%' }}>
              <Input
                value={javaFilePath}
                onChange={(e) => setJavaFilePath(e.target.value)}
                placeholder="e.g., com/rules/actions/ApprovalAction.java"
                prefix={<FileOutlined />}
              />
              <Button
                icon={<FolderOpenOutlined />}
                onClick={handleBrowseFiles}
              >
                Browse
              </Button>
            </Space.Compact>

            {loadingFile ? (
              <div style={{ marginTop: 8 }}>
                <Spin size="small" /> Loading file...
              </div>
            ) : fileExists === true ? (
              <Alert
                type="success"
                message="File exists and is readable"
                icon={<CheckCircleOutlined />}
                showIcon
                style={{ marginTop: 8 }}
              />
            ) : fileExists === false ? (
              <Alert
                type="warning"
                message="File not found (you can still save - file may be created later)"
                icon={<WarningOutlined />}
                showIcon
                style={{ marginTop: 8 }}
              />
            ) : null}
          </Form.Item>
        </Form>

        {/* Java Code Preview */}
        <div style={{ marginTop: 24 }}>
          <h4>Java Code Preview</h4>
          <div style={{ height: 400, border: '1px solid #d9d9d9', borderRadius: 4 }}>
            <Editor
              height="100%"
              language="java"
              value={javaContent || '// Select or enter a Java file path to preview the code'}
              options={{
                readOnly: true,
                minimap: { enabled: false },
                fontSize: 13,
                lineNumbers: 'on',
                scrollBeyondLastLine: false,
                automaticLayout: true,
                theme: 'vs',
              }}
            />
          </div>
        </div>

        {/* Action Buttons */}
        <div style={{ marginTop: 24, textAlign: 'right' }}>
          <Space>
            <Button onClick={onBack}>
              Cancel
            </Button>
            <Button
              type="primary"
              onClick={handleSave}
              loading={loading}
              icon={<CheckCircleOutlined />}
            >
              Save Action
            </Button>
          </Space>
        </div>
      </Card>

      {/* File Picker Modal */}
      <Modal
        title="Select Java File"
        open={showFilePicker}
        onCancel={() => setShowFilePicker(false)}
        footer={[
          <Button key="cancel" onClick={() => setShowFilePicker(false)}>
            Cancel
          </Button>
        ]}
        width={700}
      >
        <div style={{ marginBottom: 16 }}>
          <Text type="secondary">
            Select a Java file from the project. Files are listed from the java-bridge source directory.
          </Text>
        </div>
        <List
          size="small"
          bordered
          dataSource={javaFiles}
          style={{ maxHeight: 400, overflow: 'auto' }}
          renderItem={(file) => (
            <List.Item
              style={{ cursor: 'pointer' }}
              onClick={() => handleFileSelect(file.path)}
              onMouseEnter={(e) => { e.currentTarget.style.backgroundColor = '#f0f0f0'; }}
              onMouseLeave={(e) => { e.currentTarget.style.backgroundColor = 'white'; }}
            >
              <List.Item.Meta
                avatar={<CodeOutlined style={{ fontSize: 20, color: '#1890ff' }} />}
                title={file.name}
                description={
                  <Space direction="vertical" size={0}>
                    <Text type="secondary" style={{ fontSize: 12 }}>{file.path}</Text>
                    <Text type="secondary" style={{ fontSize: 11 }}>
                      {(file.size / 1024).toFixed(1)} KB
                    </Text>
                  </Space>
                }
              />
            </List.Item>
          )}
        />
      </Modal>
    </div>
  );
};

export default ActionEditor;
