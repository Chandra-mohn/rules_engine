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
  Row,
  Col,
  Typography,
} from 'antd';
import {
  PlusOutlined,
  EditOutlined,
  DeleteOutlined,
  SearchOutlined,
  PlayCircleOutlined,
  HistoryOutlined,
  ThunderboltOutlined,
  ArrowUpOutlined,
} from '@ant-design/icons';
import { rulesApi } from '../services/api';
import SchemaSelector from './SchemaSelector';
import CacheDebugger from './CacheDebugger';
import suggestionCache from '../services/suggestionCache';
import RulesTreeNavigation from './RulesTreeNavigation';

const { Search } = Input;
const { Option } = Select;
const { Title } = Typography;

const RulesListEnhanced = ({ onEditRule, onCreateRule }) => {
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
    schema_version: 'modern',
    client_id: null,
    process_group_id: null,
    process_area_id: null,
  });
  const [selectedTreeKeys, setSelectedTreeKeys] = useState([]);
  const [currentHierarchy, setCurrentHierarchy] = useState(null);
  const [showCacheDebugger, setShowCacheDebugger] = useState(false);

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

  // Initial load and preload suggestions cache
  useEffect(() => {
    loadRules();
    
    // Preload suggestions cache in background
    suggestionCache.preload().catch(error => {
      console.warn('Failed to preload suggestions cache:', error);
    });
  }, []);

  // Keyboard listener for Ctrl+D to toggle cache debugger
  useEffect(() => {
    const handleKeyPress = (event) => {
      if (event.ctrlKey && event.key === 'd') {
        event.preventDefault();
        setShowCacheDebugger(prev => !prev);
      }
    };

    document.addEventListener('keydown', handleKeyPress);
    return () => document.removeEventListener('keydown', handleKeyPress);
  }, [showCacheDebugger]);

  // Handle pagination change
  const handleTableChange = (paginationInfo) => {
    loadRules(paginationInfo.current, paginationInfo.pageSize, filters);
  };

  // Handle delete
  const handleDelete = (rule) => {
    Modal.confirm({
      title: 'Delete Rule',
      content: `Are you sure you want to delete the rule "${rule.name}"?`,
      onOk: async () => {
        try {
          await rulesApi.deleteRule(rule.id);
          message.success('Rule deleted successfully');
          loadRules();
        } catch (error) {
          message.error('Failed to delete rule: ' + (error.response?.data?.error || error.message));
        }
      },
    });
  };

  // Handle view history
  const handleViewHistory = async (rule) => {
    try {
      const response = await rulesApi.getRuleHistory(rule.id);
      const history = response.data;
      
      Modal.info({
        title: `Rule History: ${rule.name}`,
        width: 800,
        content: (
          <div>
            <p>Rule: <strong>{rule.name}</strong></p>
            <p>Version history will be displayed here.</p>
          </div>
        ),
      });
    } catch (error) {
      message.error('Failed to load rule history: ' + (error.response?.data?.error || error.message));
    }
  };

  // Get status color for new status values
  const getStatusColor = (status) => {
    switch (status) {
      case 'PROD': return 'green';
      case 'SCHD': return 'blue';
      case 'PEND': return 'orange';
      case 'VALID': return 'purple';
      case 'DRAFT': return 'default';
      default: return 'default';
    }
  };
  
  // Get status text
  const getStatusText = (status) => {
    switch (status) {
      case 'PROD': return 'Production';
      case 'SCHD': return 'Scheduled';
      case 'PEND': return 'Pending Approval';
      case 'VALID': return 'Validated';
      case 'DRAFT': return 'Draft';
      default: return status;
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

  // Handle test
  const handleTest = (rule) => {
    message.info(`Testing rule: ${rule.name}`);
  };
  
  // Handle execute
  const handleExecute = (rule) => {
    if (rule.status !== 'PROD') {
      message.error('Rule execution is only allowed for PROD status rules');
      return;
    }
    message.info('Execute functionality not yet implemented');
  };
  
  // Check if status can be promoted
  const canPromoteStatus = (currentStatus) => {
    const promotableStatuses = ['VALID', 'PEND', 'SCHD'];
    return promotableStatuses.includes(currentStatus);
  };
  
  // Handle status promotion
  const handlePromoteStatus = async (rule) => {
    const statusTransitions = {
      'VALID': [{ value: 'PEND', label: 'Submit for Approval' }],
      'PEND': [
        { value: 'SCHD', label: 'Approve (Schedule for Deployment)' },
        { value: 'VALID', label: 'Reject (Back to Validated)' }
      ],
      'SCHD': [
        { value: 'PROD', label: 'Deploy to Production' },
        { value: 'VALID', label: 'Cancel (Back to Validated)' }
      ]
    };
    
    const transitions = statusTransitions[rule.status] || [];
    
    if (transitions.length === 1) {
      // Single transition - promote directly
      const transition = transitions[0];
      try {
        await rulesApi.promoteRuleStatus(rule.id, transition.value);
        message.success(`Rule status updated to ${getStatusText(transition.value)}`);
        loadRules();
      } catch (error) {
        message.error('Failed to promote rule status: ' + (error.response?.data?.error || error.message));
      }
    } else if (transitions.length > 1) {
      // Multiple options - show modal
      Modal.confirm({
        title: `Promote Rule: ${rule.name}`,
        content: (
          <div>
            <p>Current Status: <Tag color={getStatusColor(rule.status)}>{getStatusText(rule.status)}</Tag></p>
            <p>Choose promotion option:</p>
          </div>
        ),
        okText: 'Approve',
        cancelText: 'Reject',
        onOk: async () => {
          try {
            await rulesApi.promoteRuleStatus(rule.id, 'SCHD');
            message.success('Rule approved and scheduled for deployment');
            loadRules();
          } catch (error) {
            message.error('Failed to promote rule: ' + (error.response?.data?.error || error.message));
          }
        },
        onCancel: async () => {
          try {
            await rulesApi.promoteRuleStatus(rule.id, 'VALID');
            message.success('Rule rejected and returned to validated status');
            loadRules();
          } catch (error) {
            message.error('Failed to reject rule: ' + (error.response?.data?.error || error.message));
          }
        }
      });
    }
  };

  // Handle tree navigation
  const handleTreeNodeSelect = (selectedKeys, { node }) => {
    setSelectedTreeKeys(selectedKeys);
    
    // Update filters based on selection
    let newFilters = { ...filters };
    
    if (node.type === 'client') {
      newFilters.client_id = node.id;
      newFilters.process_group_id = null;
      newFilters.process_area_id = null;
      setCurrentHierarchy({ type: 'client', name: node.name, code: node.code });
    } else if (node.type === 'process_group') {
      newFilters.process_group_id = node.id;
      newFilters.process_area_id = null;
      setCurrentHierarchy({ type: 'process_group', name: node.name, code: node.code });
    } else if (node.type === 'process_area') {
      newFilters.process_area_id = node.id;
      setCurrentHierarchy({ type: 'process_area', name: node.name, code: node.code });
    } else if (node.type === 'rule') {
      // Open rule for editing
      onEditRule({ id: node.id });
      return;
    }
    
    setFilters(newFilters);
    loadRules(1, pagination.pageSize, newFilters);
  };
  
  // Handle breadcrumb navigation
  const handleBreadcrumbClick = (item) => {
    handleTreeNodeSelect([`${item.type}-${item.id}`], {
      node: { type: item.type, id: item.id, name: item.name, code: item.code }
    });
  };

  // Table columns with hierarchy information
  const columns = [
    {
      title: 'Rule Name',
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
          <div style={{ fontSize: '11px', color: '#999', marginTop: 2 }}>
            {record.client_code} → {record.process_group_code} → {record.process_area_code}
          </div>
        </div>
      ),
    },
    {
      title: 'Status',
      dataIndex: 'status',
      key: 'status',
      width: 120,
      render: (status) => (
        <Tag color={getStatusColor(status)}>
          {getStatusText(status)}
        </Tag>
      ),
    },
    {
      title: 'Effective Date',
      dataIndex: 'effective_date',
      key: 'effective_date',
      width: 120,
      render: (date) => date ? new Date(date).toLocaleDateString() : '—',
    },
    {
      title: 'Hierarchy',
      key: 'hierarchy',
      width: 200,
      render: (_, record) => (
        <div style={{ fontSize: '12px' }}>
          <div style={{ fontWeight: 'bold' }}>{record.client_name || record.client_code}</div>
          <div style={{ color: '#666' }}>{record.process_group_name || record.process_group_code}</div>
          <div style={{ color: '#999' }}>{record.process_area_name || record.process_area_code}</div>
        </div>
      ),
    },
    {
      title: 'Validation',
      dataIndex: 'validation_status',
      key: 'validation_status',
      width: 100,
      render: (status) => (
        <Tag color={getValidationColor(status)} size="small">
          {status?.toUpperCase()}
        </Tag>
      ),
    },
    {
      title: 'Actions',
      key: 'actions',
      width: 240,
      render: (_, record) => (
        <Space>
          <Tooltip title="Edit Rule">
            <Button
              icon={<EditOutlined />}
              size="small"
              onClick={() => onEditRule(record)}
            />
          </Tooltip>
          <Tooltip title="Test Rule">
            <Button
              icon={<PlayCircleOutlined />}
              size="small"
              onClick={() => handleTest(record)}
            />
          </Tooltip>
          <Tooltip title={record.status === 'PROD' ? 'Execute Rule' : 'Execute (PROD only)'}>
            <Button
              icon={<ThunderboltOutlined />}
              size="small"
              onClick={() => handleExecute(record)}
              disabled={record.status !== 'PROD'}
              style={{
                color: record.status === 'PROD' ? '#1890ff' : undefined,
                borderColor: record.status === 'PROD' ? '#1890ff' : undefined
              }}
            />
          </Tooltip>
          {canPromoteStatus(record.status) && (
            <Tooltip title="Promote Status">
              <Button
                icon={<ArrowUpOutlined />}
                size="small"
                onClick={() => handlePromoteStatus(record)}
                style={{ color: '#52c41a', borderColor: '#52c41a' }}
              />
            </Tooltip>
          )}
          <Tooltip title="View History">
            <Button
              icon={<HistoryOutlined />}
              size="small"
              onClick={() => handleViewHistory(record)}
            />
          </Tooltip>
          <Tooltip title="Delete Rule">
            <Button
              icon={<DeleteOutlined />}
              size="small"
              danger
              onClick={() => handleDelete(record)}
            />
          </Tooltip>
        </Space>
      ),
    },
  ];

  return (
    <Row gutter={16} style={{ height: '100vh', padding: 16 }}>
      {/* Tree Navigation */}
      <Col span={6}>
        <RulesTreeNavigation 
          onNodeSelect={handleTreeNodeSelect}
          selectedKeys={selectedTreeKeys}
          onBreadcrumbClick={handleBreadcrumbClick}
        />
      </Col>
      
      {/* Rules Table */}
      <Col span={18}>
        <div>
          {/* Header */}
          <div style={{
            marginBottom: 16,
            display: 'flex',
            justifyContent: 'space-between',
            alignItems: 'center',
          }}>
            <div>
              <Title level={4} style={{ margin: 0 }}>
                Rules Management
                {currentHierarchy && (
                  <span style={{ fontSize: '14px', fontWeight: 'normal', marginLeft: 8, color: '#666' }}>
                    - {currentHierarchy.code ? `${currentHierarchy.code} (${currentHierarchy.name})` : currentHierarchy.name}
                  </span>
                )}
              </Title>
            </div>
            <Button
              type="primary"
              icon={<PlusOutlined />}
              onClick={onCreateRule}
            >
              Create Rule
            </Button>
          </div>

          {/* Filters */}
          <Space style={{ marginBottom: 16 }}>
            <Search
              placeholder="Search rules..."
              allowClear
              value={filters.search}
              onChange={(e) => {
                const newFilters = { ...filters, search: e.target.value };
                setFilters(newFilters);
                loadRules(1, pagination.pageSize, newFilters);
              }}
              style={{ width: 300 }}
            />

            <Select
              style={{ width: 150 }}
              placeholder="Status"
              allowClear
              value={filters.status}
              onChange={(value) => {
                const newFilters = { ...filters, status: value };
                setFilters(newFilters);
                loadRules(1, pagination.pageSize, newFilters);
              }}
            >
              <Option value="PROD">Production</Option>
              <Option value="SCHD">Scheduled</Option>
              <Option value="PEND">Pending Approval</Option>
              <Option value="VALID">Validated</Option>
              <Option value="DRAFT">Draft</Option>
            </Select>

            <SchemaSelector
              selectedSchema={filters.schema_version}
              onSchemaChange={(value) => {
                const newFilters = { ...filters, schema_version: value };
                setFilters(newFilters);
                loadRules(1, pagination.pageSize, newFilters);
              }}
            />
          </Space>

          {/* Rules Table */}
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
            size="small"
            scroll={{ y: 600 }}
          />

          {/* Cache Debugger */}
          {showCacheDebugger && (
            <CacheDebugger />
          )}
        </div>
      </Col>
    </Row>
  );
};

export default RulesListEnhanced;