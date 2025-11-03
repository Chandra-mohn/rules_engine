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
  Row,
  Col,
  Typography,
  Divider,
} from 'antd';
import {
  PlusOutlined,
  EditOutlined,
  DeleteOutlined,
  PlayCircleOutlined,
  HistoryOutlined,
  ThunderboltOutlined,
  ArrowUpOutlined,
  BarChartOutlined,
  DatabaseOutlined,
} from '@ant-design/icons';
import { rulesApi } from '../services/api';
import CacheDebugger from './CacheDebugger';
import suggestionCache from '../services/suggestionCache';
import RulesTreeNavigation from './RulesTreeNavigation';
import ContextManager from './ContextManager';

const { Search } = Input;
const { Option } = Select;
const { Title } = Typography;

const RulesListEnhanced = ({ onEditRule, onCreateRule, onGapAnalysis }) => {
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
    client_id: null,
    process_group_id: null,
    process_area_id: null,
    item_type: null,
  });
  const [selectedTreeKeys, setSelectedTreeKeys] = useState([]);
  const [currentHierarchy, setCurrentHierarchy] = useState(null);
  const [showCacheDebugger, setShowCacheDebugger] = useState(false);
  const [showSpecialButtons, setShowSpecialButtons] = useState(true);
  const [contextManagerVisible, setContextManagerVisible] = useState(false);

  // Load rules data
  const loadRules = async (page = 1, pageSize = 10, filterParams = {}) => {
    setLoading(true);
    try {
      const params = {
        page,
        limit: pageSize,
        // Load both rules and actionsets - let the backend filter by hierarchy
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
      // Failed to preload suggestions cache
    });
  }, []);

  // Keyboard listeners for Ctrl+D (cache debugger) and Ctrl+M (special buttons)
  useEffect(() => {
    const handleKeyPress = (event) => {
      if (event.ctrlKey && event.key === 'd') {
        event.preventDefault();
        setShowCacheDebugger(prev => !prev);
      }
      if (event.ctrlKey && event.key === 'm') {
        event.preventDefault();
        setShowSpecialButtons(prev => !prev);
      }
    };

    document.addEventListener('keydown', handleKeyPress);
    return () => document.removeEventListener('keydown', handleKeyPress);
  }, [showCacheDebugger, showSpecialButtons]);

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
      await rulesApi.getRuleHistory(rule.id);

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
      newFilters.item_type = null; // Clear item type filter
      setCurrentHierarchy({ type: 'client', name: node.name, code: node.code });
    } else if (node.type === 'process_group') {
      newFilters.process_group_id = node.id;
      newFilters.process_area_id = null;
      newFilters.item_type = null; // Clear item type filter
      setCurrentHierarchy({ type: 'process_group', name: node.name, code: node.code });
    } else if (node.type === 'process_area') {
      newFilters.process_area_id = node.id;
      newFilters.item_type = null; // Clear item type filter
      setCurrentHierarchy({ type: 'process_area', name: node.name, code: node.code });
    } else if (node.type === 'synthetic_rules') {
      // Filter to show only rules for this process area
      newFilters.process_area_id = node.parent_process_area_id;
      newFilters.item_type = 'rule';
      setCurrentHierarchy({
        type: 'synthetic_rules',
        name: node.parent_process_area_name,
        code: node.parent_process_area_name,
        synthetic_type: 'Rules'
      });
    } else if (node.type === 'synthetic_actionsets') {
      // Filter to show only actionsets for this process area
      newFilters.process_area_id = node.parent_process_area_id;
      newFilters.item_type = 'actionset';
      setCurrentHierarchy({
        type: 'synthetic_actionsets',
        name: node.parent_process_area_name,
        code: node.parent_process_area_name,
        synthetic_type: 'ActionSets'
      });
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
      title: 'Name',
      dataIndex: 'name',
      key: 'name',
      width: 300,
      sorter: true,
      render: (text, record) => (
        <div>
          <div style={{ fontWeight: 'bold' }}>
            <span style={{
              display: 'inline-block',
              width: '16px',
              height: '16px',
              backgroundColor:
                record.item_type === 'actionset' ? '#1890ff' :
                record.item_type === 'action' ? '#13c2c2' :
                record.item_type === 'mon_rule' ? '#faad14' :
                record.item_type === 'non_mon_rule' ? '#722ed1' : '#000',
              color: 'white',
              fontSize: '10px',
              textAlign: 'center',
              lineHeight: '16px',
              marginRight: '8px',
              fontWeight: 'bold'
            }}>
              {record.item_type === 'actionset' ? 'AS' :
               record.item_type === 'action' ? 'AC' :
               record.item_type === 'mon_rule' ? 'M' :
               record.item_type === 'non_mon_rule' ? 'N' : 'R'}
            </span>
            {text}
          </div>
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
      title: 'Type',
      dataIndex: 'item_type',
      key: 'item_type',
      width: 100,
      render: (type) => {
        const typeMap = {
          'rule': { label: 'Rule', color: 'green' },
          'actionset': { label: 'ActionSet', color: 'blue' },
          'action': { label: 'Action', color: 'cyan' },
          'mon_rule': { label: 'Monetary', color: 'gold' },
          'non_mon_rule': { label: 'Non-Monetary', color: 'purple' }
        };
        const typeInfo = typeMap[type] || { label: 'Unknown', color: 'gray' };
        return (
          <Tag color={typeInfo.color}>
            {typeInfo.label}
          </Tag>
        );
      },
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
      title: 'Actions',
      key: 'actions',
      width: 240,
      render: (_, record) => (
        <Space>
          <Tooltip title={`Edit ${record.item_type === 'actionset' ? 'ActionSet' : record.item_type === 'action' ? 'Action' : 'Rule'}`}>
            <Button
              icon={<EditOutlined />}
              size="small"
              onClick={() => onEditRule(record)}
            />
          </Tooltip>
          {record.item_type !== 'action' && (
            <>
              <Tooltip title={`Test ${record.item_type === 'actionset' ? 'ActionSet' : 'Rule'}`}>
                <Button
                  icon={<PlayCircleOutlined />}
                  size="small"
                  onClick={() => handleTest(record)}
                />
              </Tooltip>
              <Tooltip title={record.status === 'PROD' ? `Execute ${record.item_type === 'actionset' ? 'ActionSet' : 'Rule'}` : 'Execute (PROD only)'}>
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
            </>
          )}
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
          <div style={{ marginBottom: 16 }}>
            <div style={{ marginBottom: 12 }}>
              <Title level={4} style={{ margin: 0 }}>
                {currentHierarchy && currentHierarchy.synthetic_type ? currentHierarchy.synthetic_type : ''}
                {currentHierarchy && (
                  <span style={{ fontSize: '14px', fontWeight: 'normal', marginLeft: 8, color: '#666' }}>
                    {currentHierarchy.synthetic_type
                        ? `${currentHierarchy.name}`
                        : (currentHierarchy.code ? `${currentHierarchy.code} (${currentHierarchy.name})` : currentHierarchy.name)
                    }
                  </span>
                )}
              </Title>
            </div>
            <Space>
              <Button
                icon={<PlusOutlined />}
                onClick={() => onCreateRule('rule')}
                className="action-button action-button-rule"
                style={{
                  height: '36px',
                  minWidth: '140px',
                  backgroundColor: '#f5f7fa',
                  borderColor: '#4a90b8',
                  color: '#4a90b8',
                  fontWeight: 500,
                  borderRadius: '6px',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center'
                }}
                onMouseEnter={(e) => {
                  e.target.style.backgroundColor = '#4a90b8';
                  e.target.style.color = 'white';
                }}
                onMouseLeave={(e) => {
                  e.target.style.backgroundColor = '#f5f7fa';
                  e.target.style.color = '#4a90b8';
                }}
              >
                New Rule
              </Button>
              <Button
                icon={<PlusOutlined />}
                onClick={() => onCreateRule('actionset')}
                className="action-button action-button-actionset"
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
                New ActionSet
              </Button>
              {showSpecialButtons && (
                <>
                  <Button
                    icon={<PlusOutlined />}
                    onClick={() => onCreateRule('non_mon_rule')}
                    className="action-button action-button-non-monetary"
                    style={{
                      height: '36px',
                      minWidth: '140px',
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
                    New Non-Monetary
                  </Button>
                  <Button
                    icon={<PlusOutlined />}
                    onClick={() => onCreateRule('mon_rule')}
                    className="action-button action-button-monetary"
                    style={{
                      height: '36px',
                      minWidth: '140px',
                      backgroundColor: '#f2f8f5',
                      borderColor: '#5a8a6b',
                      color: '#5a8a6b',
                      fontWeight: 500,
                      borderRadius: '6px',
                      display: 'flex',
                      alignItems: 'center',
                      justifyContent: 'center'
                    }}
                    onMouseEnter={(e) => {
                      e.target.style.backgroundColor = '#5a8a6b';
                      e.target.style.color = 'white';
                    }}
                    onMouseLeave={(e) => {
                      e.target.style.backgroundColor = '#f2f8f5';
                      e.target.style.color = '#5a8a6b';
                    }}
                  >
                    New Monetary
                  </Button>
                </>
              )}
              <Divider type="vertical" style={{ height: '36px', borderColor: '#d9d9d9' }} />
              <Button
                icon={<PlusOutlined />}
                onClick={() => onCreateRule('action')}
                className="action-button action-button-action"
                style={{
                  height: '36px',
                  minWidth: '140px',
                  backgroundColor: '#f0f9ff',
                  borderColor: '#0ea5e9',
                  color: '#0ea5e9',
                  fontWeight: 500,
                  borderRadius: '6px',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center'
                }}
                onMouseEnter={(e) => {
                  e.target.style.backgroundColor = '#0ea5e9';
                  e.target.style.color = 'white';
                }}
                onMouseLeave={(e) => {
                  e.target.style.backgroundColor = '#f0f9ff';
                  e.target.style.color = '#0ea5e9';
                }}
              >
                New Action
              </Button>
              <Button
                icon={<DatabaseOutlined />}
                onClick={() => setContextManagerVisible(true)}
                className="action-button action-button-context"
                style={{
                  height: '36px',
                  minWidth: '160px',
                  backgroundColor: '#f0fdf4',
                  borderColor: '#22c55e',
                  color: '#16a34a',
                  fontWeight: 500,
                  borderRadius: '6px',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center'
                }}
                onMouseEnter={(e) => {
                  e.target.style.backgroundColor = '#22c55e';
                  e.target.style.color = 'white';
                }}
                onMouseLeave={(e) => {
                  e.target.style.backgroundColor = '#f0fdf4';
                  e.target.style.color = '#16a34a';
                }}
              >
                Manage Contexts
              </Button>
              <Divider type="vertical" style={{ height: '36px', borderColor: '#d9d9d9' }} />
              <Button
                icon={<BarChartOutlined />}
                onClick={onGapAnalysis}
                className="action-button gap-analysis-button"
                style={{
                  height: '36px',
                  minWidth: '140px',
                  backgroundColor: '#fff7e6',
                  borderColor: '#d48806',
                  color: '#d48806',
                  fontWeight: 500,
                  borderRadius: '6px',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center'
                }}
                onMouseEnter={(e) => {
                  e.target.style.backgroundColor = '#d48806';
                  e.target.style.color = 'white';
                }}
                onMouseLeave={(e) => {
                  e.target.style.backgroundColor = '#fff7e6';
                  e.target.style.color = '#d48806';
                }}
              >
                Gap Analysis
              </Button>
            </Space>
          </div>

          {/* Filters */}
          <Space style={{ marginBottom: 16 }}>
            <Search
              placeholder="Search rules & actionsets..."
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

          {/* Context Manager Modal */}
          <ContextManager
            visible={contextManagerVisible}
            onClose={() => setContextManagerVisible(false)}
            onContextCreated={() => {
              message.success('Context created successfully!');
            }}
          />
        </div>
      </Col>
    </Row>
  );
};

export default RulesListEnhanced;