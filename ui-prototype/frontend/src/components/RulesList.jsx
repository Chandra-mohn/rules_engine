import React, { useState, useEffect } from 'react';
import {
  Table,
  Button,
  Space,
  Input,
  Select,
  Tag,
  Modal,
  message,
  Tooltip,
  Card,
} from 'antd';
import {
  PlusOutlined,
  EditOutlined,
  DeleteOutlined,
  SearchOutlined,
  PlayCircleOutlined,
  HistoryOutlined,
} from '@ant-design/icons';
import { rulesApi } from '../services/api';

const { Search } = Input;
const { Option } = Select;

const RulesList = ({ onEditRule, onCreateRule }) => {
  const [rules, setRules] = useState([]);
  const [loading, setLoading] = useState(false);
  const [pagination, setPagination] = useState({
    current: 1,
    pageSize: 10,
    total: 0,
  });
  const [filters, setFilters] = useState({
    status: null,
    search: '',
  });

  // Load rules data
  const loadRules = async (page = 1, pageSize = 10, filterParams = {}) => {
    setLoading(true);
    try {
      const params = {
        page,
        limit: pageSize,
        ...filterParams,
      };

      const response = await rulesApi.getRules(params);
      const { rules: rulesData, total, page: currentPage } = response.data;

      setRules(rulesData);
      setPagination({
        current: currentPage,
        pageSize,
        total,
      });
    } catch (error) {
      message.error('Failed to load rules: ' + (error.response?.data?.error || error.message));
    } finally {
      setLoading(false);
    }
  };

  // Initial load
  useEffect(() => {
    loadRules();
  }, []);

  // Handle pagination change
  const handleTableChange = (paginationInfo) => {
    loadRules(paginationInfo.current, paginationInfo.pageSize, filters);
  };

  // Handle search
  const handleSearch = (value) => {
    const newFilters = { ...filters, search: value };
    setFilters(newFilters);
    loadRules(1, pagination.pageSize, newFilters);
  };

  // Handle status filter change
  const handleStatusFilter = (value) => {
    const newFilters = { ...filters, status: value };
    setFilters(newFilters);
    loadRules(1, pagination.pageSize, newFilters);
  };

  // Handle delete rule
  const handleDelete = (rule) => {
    Modal.confirm({
      title: 'Delete Rule',
      content: `Are you sure you want to delete the rule "${rule.name}"?`,
      okText: 'Delete',
      okType: 'danger',
      cancelText: 'Cancel',
      onOk: async () => {
        try {
          await rulesApi.deleteRule(rule.id);
          message.success('Rule deleted successfully');
          loadRules(pagination.current, pagination.pageSize, filters);
        } catch (error) {
          message.error('Failed to delete rule: ' + (error.response?.data?.error || error.message));
        }
      },
    });
  };

  // Handle test rule
  const handleTest = (rule) => {
    // For now, show a simple test modal
    // In a full implementation, this would open a test interface
    Modal.info({
      title: 'Test Rule',
      content: (
        <div>
          <p>Rule: <strong>{rule.name}</strong></p>
          <p>Testing functionality will be implemented in the full version.</p>
          <p>This would allow you to:</p>
          <ul>
            <li>Provide sample input data</li>
            <li>Execute the rule against the data</li>
            <li>View the execution results</li>
          </ul>
        </div>
      ),
    });
  };

  // Handle view history
  const handleHistory = async (rule) => {
    try {
      const response = await rulesApi.getRuleHistory(rule.id);
      const history = response.data;

      Modal.info({
        title: `Rule History: ${rule.name}`,
        width: 800,
        content: (
          <div>
            {history.length === 0 ? (
              <p>No history available</p>
            ) : (
              <div style={{ maxHeight: 400, overflowY: 'auto' }}>
                {history.map((entry, index) => (
                  <Card key={entry.id} size="small" style={{ marginBottom: 8 }}>
                    <div>
                      <strong>Version {entry.version}</strong> - {entry.created_at}
                    </div>
                    <div>By: {entry.created_by}</div>
                    {entry.change_reason && <div>Reason: {entry.change_reason}</div>}
                  </Card>
                ))}
              </div>
            )}
          </div>
        ),
      });
    } catch (error) {
      message.error('Failed to load rule history: ' + (error.response?.data?.error || error.message));
    }
  };

  // Get status tag color
  const getStatusColor = (status) => {
    switch (status) {
      case 'active': return 'green';
      case 'draft': return 'orange';
      case 'inactive': return 'red';
      case 'error': return 'red';
      default: return 'default';
    }
  };

  // Get validation status tag color
  const getValidationColor = (status) => {
    switch (status) {
      case 'valid': return 'green';
      case 'invalid': return 'red';
      case 'pending': return 'orange';
      default: return 'default';
    }
  };

  // Table columns
  const columns = [
    {
      title: 'Name',
      dataIndex: 'name',
      key: 'name',
      sorter: true,
      render: (text, record) => (
        <div>
          <div style={{ fontWeight: 'bold' }}>{text}</div>
          {record.description && (
            <div style={{ fontSize: '12px', color: '#666' }}>
              {record.description}
            </div>
          )}
        </div>
      ),
    },
    {
      title: 'Status',
      dataIndex: 'status',
      key: 'status',
      width: 100,
      render: (status) => (
        <Tag color={getStatusColor(status)}>
          {status.toUpperCase()}
        </Tag>
      ),
    },
    {
      title: 'Validation',
      dataIndex: 'validation_status',
      key: 'validation_status',
      width: 120,
      render: (status, record) => (
        <Tooltip title={record.validation_message}>
          <Tag color={getValidationColor(status)}>
            {status.toUpperCase()}
          </Tag>
        </Tooltip>
      ),
    },
    {
      title: 'Version',
      dataIndex: 'version',
      key: 'version',
      width: 80,
      align: 'center',
    },
    {
      title: 'Updated',
      dataIndex: 'updated_at',
      key: 'updated_at',
      width: 150,
      render: (date) => new Date(date).toLocaleDateString(),
    },
    {
      title: 'Actions',
      key: 'actions',
      width: 200,
      render: (_, record) => (
        <Space size="small">
          <Tooltip title="Edit Rule">
            <Button
              type="text"
              icon={<EditOutlined />}
              onClick={() => onEditRule(record)}
            />
          </Tooltip>
          <Tooltip title="Test Rule">
            <Button
              type="text"
              icon={<PlayCircleOutlined />}
              onClick={() => handleTest(record)}
            />
          </Tooltip>
          <Tooltip title="View History">
            <Button
              type="text"
              icon={<HistoryOutlined />}
              onClick={() => handleHistory(record)}
            />
          </Tooltip>
          <Tooltip title="Delete Rule">
            <Button
              type="text"
              danger
              icon={<DeleteOutlined />}
              onClick={() => handleDelete(record)}
            />
          </Tooltip>
        </Space>
      ),
    },
  ];

  return (
    <div>
      {/* Header */}
      <div style={{ marginBottom: 16, display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
        <h2>Rules Management</h2>
        <Button
          type="primary"
          icon={<PlusOutlined />}
          onClick={onCreateRule}
        >
          Create Rule
        </Button>
      </div>

      {/* Filters */}
      <div style={{ marginBottom: 16, display: 'flex', gap: 16 }}>
        <Search
          placeholder="Search rules..."
          allowClear
          style={{ width: 300 }}
          onSearch={handleSearch}
          enterButton={<SearchOutlined />}
        />
        <Select
          placeholder="Filter by status"
          allowClear
          style={{ width: 150 }}
          onChange={handleStatusFilter}
        >
          <Option value="active">Active</Option>
          <Option value="draft">Draft</Option>
          <Option value="inactive">Inactive</Option>
          <Option value="error">Error</Option>
        </Select>
      </div>

      {/* Table */}
      <Table
        columns={columns}
        dataSource={rules}
        rowKey="id"
        loading={loading}
        pagination={{
          ...pagination,
          showSizeChanger: true,
          showQuickJumper: true,
          showTotal: (total, range) =>
            `${range[0]}-${range[1]} of ${total} rules`,
        }}
        onChange={handleTableChange}
        size="middle"
      />
    </div>
  );
};

export default RulesList;