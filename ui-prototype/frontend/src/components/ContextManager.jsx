import React, { useState, useEffect } from 'react';
import {
  Modal,
  Table,
  Button,
  Space,
  message,
  Popconfirm,
  Tag,
  Input,
  Form,
  Select,
  Tooltip,
  Card,
  Tabs,
  Descriptions,
} from 'antd';
import {
  PlusOutlined,
  EditOutlined,
  DeleteOutlined,
  CopyOutlined,
  EyeOutlined,
} from '@ant-design/icons';
import { contextsApi } from '../services/api';

const { TextArea } = Input;
const { TabPane } = Tabs;

/**
 * ContextManager - Full CRUD management for rule contexts
 * Handles both Schema Templates and Test Contexts
 */
const ContextManager = ({ visible, onClose, onContextCreated }) => {
  const [contexts, setContexts] = useState([]);
  const [loading, setLoading] = useState(false);
  const [editModalVisible, setEditModalVisible] = useState(false);
  const [viewModalVisible, setViewModalVisible] = useState(false);
  const [currentContext, setCurrentContext] = useState(null);
  const [form] = Form.useForm();
  const [activeTab, setActiveTab] = useState('all');

  useEffect(() => {
    if (visible) {
      loadContexts();
    }
  }, [visible]);

  const loadContexts = async () => {
    setLoading(true);
    try {
      const response = await contextsApi.getContexts({ limit: 100 });
      setContexts(response.data.contexts || []);
    } catch (error) {
      message.error('Failed to load contexts: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleCreate = () => {
    form.resetFields();
    setCurrentContext(null);
    setEditModalVisible(true);
  };

  const handleEdit = (context) => {
    setCurrentContext(context);
    form.setFieldsValue({
      name: context.name,
      description: context.description,
      is_schema_template: context.is_schema_template,
      version: context.version,
      context_data: JSON.stringify(context.context_data, null, 2),
    });
    setEditModalVisible(true);
  };

  const handleView = (context) => {
    setCurrentContext(context);
    setViewModalVisible(true);
  };

  const handleClone = async (context) => {
    const newName = prompt(`Clone "${context.name}" as:`, `${context.name} (Copy)`);
    if (!newName) return;

    try {
      await contextsApi.cloneContext(context.id, newName, `Cloned from ${context.name}`);
      message.success('Context cloned successfully');
      loadContexts();
      if (onContextCreated) onContextCreated();
    } catch (error) {
      message.error('Failed to clone context: ' + error.response?.data?.error || error.message);
    }
  };

  const handleDelete = async (id) => {
    try {
      await contextsApi.deleteContext(id);
      message.success('Context deleted successfully');
      loadContexts();
    } catch (error) {
      message.error('Failed to delete context: ' + error.response?.data?.error || error.message);
    }
  };

  const handleSave = async (values) => {
    try {
      // Parse JSON context data
      const contextData = JSON.parse(values.context_data);

      const payload = {
        name: values.name,
        description: values.description,
        context_data: contextData,
        is_schema_template: values.is_schema_template || false,
        version: values.version,
      };

      if (currentContext) {
        await contextsApi.updateContext(currentContext.id, payload);
        message.success('Context updated successfully');
      } else {
        await contextsApi.createContext(payload);
        message.success('Context created successfully');
        if (onContextCreated) onContextCreated();
      }

      setEditModalVisible(false);
      loadContexts();
    } catch (error) {
      if (error instanceof SyntaxError) {
        message.error('Invalid JSON in context data');
      } else {
        message.error('Failed to save context: ' + error.response?.data?.error || error.message);
      }
    }
  };

  const filteredContexts = contexts.filter((ctx) => {
    if (activeTab === 'templates') return ctx.is_schema_template;
    if (activeTab === 'test') return !ctx.is_schema_template;
    return true;
  });

  const columns = [
    {
      title: 'Name',
      dataIndex: 'name',
      key: 'name',
      width: '30%',
      render: (text, record) => (
        <Space>
          {text}
          {record.is_schema_template && <Tag color="blue">Schema Template</Tag>}
        </Space>
      ),
    },
    {
      title: 'Description',
      dataIndex: 'description',
      key: 'description',
      width: '35%',
      ellipsis: true,
    },
    {
      title: 'Version',
      dataIndex: 'version',
      key: 'version',
      width: '10%',
    },
    {
      title: 'Actions',
      key: 'actions',
      width: '25%',
      render: (_, record) => (
        <Space>
          <Tooltip title="View">
            <Button type="text" icon={<EyeOutlined />} onClick={() => handleView(record)} />
          </Tooltip>
          <Tooltip title="Edit">
            <Button type="text" icon={<EditOutlined />} onClick={() => handleEdit(record)} />
          </Tooltip>
          <Tooltip title="Clone">
            <Button type="text" icon={<CopyOutlined />} onClick={() => handleClone(record)} />
          </Tooltip>
          <Popconfirm
            title="Are you sure you want to delete this context?"
            onConfirm={() => handleDelete(record.id)}
            okText="Yes"
            cancelText="No"
          >
            <Tooltip title="Delete">
              <Button type="text" danger icon={<DeleteOutlined />} />
            </Tooltip>
          </Popconfirm>
        </Space>
      ),
    },
  ];

  return (
    <>
      <Modal
        title="Context Management"
        open={visible}
        onCancel={onClose}
        width={1000}
        footer={[
          <Button key="close" onClick={onClose}>
            Close
          </Button>,
          <Button key="create" type="primary" icon={<PlusOutlined />} onClick={handleCreate}>
            New Context
          </Button>,
        ]}
      >
        <Tabs activeKey={activeTab} onChange={setActiveTab}>
          <TabPane tab="All Contexts" key="all" />
          <TabPane tab="Schema Templates" key="templates" />
          <TabPane tab="Test Contexts" key="test" />
        </Tabs>

        <Table
          columns={columns}
          dataSource={filteredContexts}
          rowKey="id"
          loading={loading}
          pagination={{ pageSize: 10 }}
          size="small"
        />
      </Modal>

      {/* Create/Edit Modal */}
      <Modal
        title={currentContext ? 'Edit Context' : 'Create New Context'}
        open={editModalVisible}
        onCancel={() => setEditModalVisible(false)}
        onOk={() => form.submit()}
        width={800}
      >
        <Form form={form} layout="vertical" onFinish={handleSave}>
          <Form.Item
            name="name"
            label="Name"
            rules={[{ required: true, message: 'Please enter a name' }]}
          >
            <Input placeholder="e.g., High Credit Applicant - Approval" />
          </Form.Item>

          <Form.Item name="description" label="Description">
            <Input.TextArea rows={2} placeholder="Brief description of this context" />
          </Form.Item>

          <Form.Item name="is_schema_template" label="Type" initialValue={false}>
            <Select>
              <Select.Option value={false}>Test Context (actual test data)</Select.Option>
              <Select.Option value={true}>Schema Template (structure definition)</Select.Option>
            </Select>
          </Form.Item>

          <Form.Item name="version" label="Version">
            <Input placeholder="e.g., 1.0.0" />
          </Form.Item>

          <Form.Item
            name="context_data"
            label="Context Data (JSON)"
            rules={[
              { required: true, message: 'Please enter context data' },
              {
                validator: (_, value) => {
                  try {
                    if (value) JSON.parse(value);
                    return Promise.resolve();
                  } catch (e) {
                    return Promise.reject(new Error('Invalid JSON format'));
                  }
                },
              },
            ]}
            extra="For schema templates, include _metadata fields to define structure"
          >
            <TextArea
              rows={15}
              placeholder={`{
  "applicant": {
    "creditScore": 750,
    "age": 35,
    "_metadata": {
      "creditScore": {
        "type": "number",
        "description": "FICO credit score (300-850)"
      }
    }
  }
}`}
            />
          </Form.Item>
        </Form>
      </Modal>

      {/* View Modal */}
      <Modal
        title={`Context: ${currentContext?.name}`}
        open={viewModalVisible}
        onCancel={() => setViewModalVisible(false)}
        width={800}
        footer={[
          <Button key="close" onClick={() => setViewModalVisible(false)}>
            Close
          </Button>,
        ]}
      >
        {currentContext && (
          <Card>
            <Descriptions column={1} bordered size="small">
              <Descriptions.Item label="Name">{currentContext.name}</Descriptions.Item>
              <Descriptions.Item label="Description">
                {currentContext.description || <em>No description</em>}
              </Descriptions.Item>
              <Descriptions.Item label="Type">
                {currentContext.is_schema_template ? (
                  <Tag color="blue">Schema Template</Tag>
                ) : (
                  <Tag>Test Context</Tag>
                )}
              </Descriptions.Item>
              <Descriptions.Item label="Version">
                {currentContext.version || <em>No version</em>}
              </Descriptions.Item>
              <Descriptions.Item label="Created">
                {new Date(currentContext.created_at).toLocaleString()}
              </Descriptions.Item>
              <Descriptions.Item label="Updated">
                {new Date(currentContext.updated_at).toLocaleString()}
              </Descriptions.Item>
              <Descriptions.Item label="Context Data">
                <pre style={{ maxHeight: 400, overflow: 'auto', fontSize: '12px' }}>
                  {JSON.stringify(currentContext.context_data, null, 2)}
                </pre>
              </Descriptions.Item>
            </Descriptions>
          </Card>
        )}
      </Modal>
    </>
  );
};

export default ContextManager;
