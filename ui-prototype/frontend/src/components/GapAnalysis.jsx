import React, { useState, useEffect, useCallback } from 'react';
import {
  Card,
  Table,
  Button,
  Alert,
  Spin,
  Tag,
  Space,
  Row,
  Col,
  Pagination,
  Typography,
  Tooltip,
  Divider,
  Empty,
  Modal
} from 'antd';
import {
  CheckCircleOutlined,
  CloseCircleOutlined,
  InfoCircleOutlined,
  FileTextOutlined,
  SettingOutlined,
  BugOutlined,
  DollarOutlined,
  TagOutlined
} from '@ant-design/icons';
import api from '../services/api';

const { Title, Text } = Typography;

const GapAnalysis = ({ onBack }) => {
  const [loading, setLoading] = useState(true);
  const [data, setData] = useState(null);
  const [error, setError] = useState(null);
  const [activeFilters, setActiveFilters] = useState([]);
  const [currentPage, setCurrentPage] = useState(1);
  const [pageSize] = useState(20);
  const [modalVisible, setModalVisible] = useState(false);
  const [modalData, setModalData] = useState({ title: '', items: [], type: '' });

  const fetchGapAnalysis = useCallback(async () => {
    console.log('🌐 fetchGapAnalysis called with activeFilters:', activeFilters, 'currentPage:', currentPage);
    setLoading(true);
    try {
      const params = {
        page: currentPage,
        limit: pageSize
      };

      let response;
      if (activeFilters.length > 0) {
        // Flask expects multiple 'filter' parameters, not an array
        // We need to construct URLSearchParams to handle multiple values for same key
        const searchParams = new URLSearchParams();
        searchParams.set('page', currentPage);
        searchParams.set('limit', pageSize);

        activeFilters.forEach(filter => {
          searchParams.append('filter', filter);
        });

        response = await api.get(`/rules/gap-analysis?${searchParams.toString()}`);
      } else {
        response = await api.get('/rules/gap-analysis', { params });
      }
      if (response.data.success) {
        setData(response.data);
        setError(null);
      } else {
        setError(response.data.error || 'Failed to fetch gap analysis');
      }
    } catch (err) {
      setError(err.message || 'Network error occurred');
      console.error('Gap analysis fetch error:', err);
    } finally {
      setLoading(false);
    }
  }, [activeFilters, currentPage, pageSize]);

  useEffect(() => {
    fetchGapAnalysis();
  }, [fetchGapAnalysis]);



  const handleFilterClick = (filterType) => {
    setActiveFilters(prev => {
      // Define mutually exclusive filter groups
      const itemTypeFilters = ['total', 'rule', 'actionset', 'mon_rule', 'non_mon_rule', 'action'];
      const statusFilters = ['DRAFT', 'VALID', 'PROD', 'PEND', 'SCHD'];

      let newFilters = [...prev];

      // Handle mutually exclusive groups
      if (itemTypeFilters.includes(filterType)) {
        // Remove all other item type filters
        newFilters = newFilters.filter(f => !itemTypeFilters.includes(f));
        // Add the new one if it wasn't already active, otherwise leave empty (toggle off)
        if (!prev.includes(filterType)) {
          newFilters.push(filterType);
        }
      } else if (statusFilters.includes(filterType)) {
        // Remove all other status filters
        newFilters = newFilters.filter(f => !statusFilters.includes(f));
        // Add the new one if it wasn't already active
        if (!prev.includes(filterType)) {
          newFilters.push(filterType);
        }
      } else {
        // For other filters (like missing_actions), use additive logic
        if (prev.includes(filterType)) {
          newFilters = newFilters.filter(f => f !== filterType);
        } else {
          newFilters.push(filterType);
        }
      }

      return newFilters;
    });
    setCurrentPage(1); // Reset to first page when filters change
  };

  const showMissingItems = (type) => {
    if (!data?.aggregates) return;

    let title, items;
    if (type === 'actions') {
      title = 'Missing Actions/ActionSets';
      items = data.aggregates.action_analysis.all_missing_actions || [];
    } else if (type === 'attributes') {
      title = 'Missing Attributes';
      items = data.aggregates.attribute_analysis.all_missing_attributes || [];
    }

    // Add count to title for clarity
    title = `${title} (${items.length} total)`;

    setModalData({ title, items, type });
    setModalVisible(true);
  };

  const handlePageChange = (page) => {
    setCurrentPage(page);
  };

  const getStatusColor = (status) => {
    switch (status) {
      case 'valid': return 'green';
      case 'invalid': return 'red';
      case 'error': return 'orange';
      default: return 'default';
    }
  };

  const getItemTypeColor = (itemType) => {
    switch (itemType) {
      case 'rule': return 'blue';
      case 'actionset': return 'purple';
      case 'mon_rule': return 'gold';
      case 'non_mon_rule': return 'cyan';
      case 'action': return 'green';
      default: return 'default';
    }
  };

  const getItemTypeIcon = (itemType) => {
    switch (itemType) {
      case 'rule': return <FileTextOutlined />;
      case 'actionset': return <SettingOutlined />;
      case 'mon_rule': return <DollarOutlined />;
      case 'non_mon_rule': return <TagOutlined />;
      case 'action': return <BugOutlined />;
      default: return <InfoCircleOutlined />;
    }
  };

  const columns = [
    {
      title: 'Rule Name',
      dataIndex: ['rule', 'name'],
      key: 'name',
      render: (text, record) => (
        <Space>
          {getItemTypeIcon(record.rule.item_type)}
          <Text strong>{text}</Text>
        </Space>
      ),
      sorter: (a, b) => a.rule.name.localeCompare(b.rule.name),
    },
    {
      title: 'Type',
      dataIndex: ['rule', 'item_type'],
      key: 'item_type',
      render: (itemType) => (
        <Tag color={getItemTypeColor(itemType)} icon={getItemTypeIcon(itemType)}>
          {itemType.toUpperCase()}
        </Tag>
      ),
      filters: [
        { text: 'Rule', value: 'rule' },
        { text: 'ActionSet', value: 'actionset' },
        { text: 'Monetary Rule', value: 'mon_rule' },
        { text: 'Non-Monetary Rule', value: 'non_mon_rule' },
        { text: 'Action', value: 'action' },
      ],
      onFilter: (value, record) => record.rule.item_type === value,
    },
    {
      title: 'Status',
      dataIndex: ['rule', 'status'],
      key: 'status',
      render: (status) => (
        <Tag color={status === 'PROD' ? 'green' : status === 'VALID' ? 'blue' : 'orange'}>
          {status}
        </Tag>
      ),
    },
    {
      title: 'Validation',
      dataIndex: 'validation_status',
      key: 'validation_status',
      render: (status) => (
        <Tag
          color={getStatusColor(status)}
          icon={status === 'valid' ? <CheckCircleOutlined /> : <CloseCircleOutlined />}
        >
          {status.toUpperCase()}
        </Tag>
      ),
    },
    {
      title: 'Missing Actions',
      dataIndex: 'missing_actions',
      key: 'missing_actions',
      render: (missingActions) => (
        missingActions && missingActions.length > 0 ? (
          <Tooltip title={missingActions.join(', ')}>
            <Tag color="red">{missingActions.length} Missing</Tag>
          </Tooltip>
        ) : (
          <Tag color="green">None</Tag>
        )
      ),
      sorter: (a, b) => (a.missing_actions?.length || 0) - (b.missing_actions?.length || 0),
    },
    {
      title: 'Attributes Used',
      dataIndex: 'extracted_attributes',
      key: 'attributes',
      render: (attributes) => (
        <Tooltip title={(attributes || []).join(', ')}>
          <Text>{(attributes || []).length} attributes</Text>
        </Tooltip>
      ),
    },
    {
      title: 'Actions Referenced',
      dataIndex: 'referenced_actions',
      key: 'actions',
      render: (actions, record) => {
        const safeActions = actions || [];
        const safeActionsets = record.referenced_actionsets || [];
        const totalActions = safeActions.length + safeActionsets.length;
        const allActions = [...safeActions, ...safeActionsets];
        return (
          <Tooltip title={allActions.join(', ')}>
            <Text>{totalActions} actions</Text>
          </Tooltip>
        );
      },
    }
  ];

  if (loading) {
    return (
      <div style={{ textAlign: 'center', padding: '50px' }}>
        <Spin size="large" />
        <div style={{ marginTop: 16 }}>
          <Text>Analyzing rules and generating gap analysis...</Text>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <Alert
        message="Error Loading Gap Analysis"
        description={error}
        type="error"
        showIcon
        action={
          <Button size="small" onClick={fetchGapAnalysis}>
            Retry
          </Button>
        }
      />
    );
  }

  if (!data) {
    return <Empty description="No gap analysis data available" />;
  }

  if (!data.aggregates) {
    return <Empty description="Gap analysis data incomplete - missing aggregates" />;
  }

  const { aggregates, results, pagination } = data;

  return (
    <div style={{ padding: '24px' }}>
      <div style={{ marginBottom: 16 }}>
        <Button onClick={onBack} style={{ marginRight: 16 }}>
          ← Back to Rules
        </Button>
      </div>
      <Title level={2}>
        <BugOutlined style={{ marginRight: 8 }} />
        Gap Analysis Dashboard
      </Title>

      <Divider style={{ marginTop: 16, marginBottom: 16 }} />

      {/* Comprehensive Dashboard Sections */}
      <Row gutter={[24, 16]} style={{ marginBottom: 24 }}>
        {/* Rule Inventory Section */}
        <Col xs={24} lg={8}>
          <Card
            title={
              <span style={{ fontSize: '16px', fontWeight: 600 }}>
                RULE INVENTORY
              </span>
            }
            style={{ height: '220px' }}
          >
            <div style={{ fontSize: '14px', lineHeight: '1.8' }}>
              <div
                style={{ cursor: 'pointer', color: '#1890ff' }}
                onClick={() => handleFilterClick('total')}
              >
                <strong>Total Rules: {aggregates.total_rules}</strong>
              </div>
              <div>
                <div
                  style={{ cursor: 'pointer', color: aggregates.type_distribution.rule > 0 ? '#1890ff' : '#999' }}
                  onClick={() => handleFilterClick('rule')}
                >
                  Rules: {aggregates.type_distribution.rule || 0}
                </div>
                <div
                  style={{ cursor: 'pointer', color: aggregates.type_distribution.actionset > 0 ? '#722ed1' : '#999' }}
                  onClick={() => handleFilterClick('actionset')}
                >
                  ActionSets: {aggregates.type_distribution.actionset || 0}
                </div>
                <div
                  style={{ cursor: 'pointer', color: aggregates.type_distribution.mon_rule > 0 ? '#52c41a' : '#999' }}
                  onClick={() => handleFilterClick('mon_rule')}
                >
                  Monetary: {aggregates.type_distribution.mon_rule || 0}
                </div>
                <div
                  style={{ cursor: 'pointer', color: aggregates.type_distribution.non_mon_rule > 0 ? '#fa8c16' : '#999' }}
                  onClick={() => handleFilterClick('non_mon_rule')}
                >
                  Non-Monetary: {aggregates.type_distribution.non_mon_rule || 0}
                </div>
              </div>
            </div>
          </Card>
        </Col>

        {/* Rule Status Section */}
        <Col xs={24} lg={8}>
          <Card
            title={
              <span style={{ fontSize: '16px', fontWeight: 600 }}>
                RULE STATUS
              </span>
            }
            style={{ height: '220px' }}
          >
            <div style={{ fontSize: '14px', lineHeight: '1.8' }}>
              <div
                style={{ cursor: 'pointer', color: aggregates.status_distribution.VALID > 0 ? '#52c41a' : '#999' }}
                onClick={() => handleFilterClick('VALID')}
              >
                Valid: {aggregates.status_distribution.VALID || 0} ({Math.round(((aggregates.status_distribution.VALID || 0) / aggregates.total_rules) * 100)}%)
              </div>
              <div
                style={{ cursor: 'pointer', color: aggregates.status_distribution.DRAFT > 0 ? '#fa8c16' : '#999' }}
                onClick={() => handleFilterClick('DRAFT')}
              >
                Draft: {aggregates.status_distribution.DRAFT || 0} ({Math.round(((aggregates.status_distribution.DRAFT || 0) / aggregates.total_rules) * 100)}%)
              </div>
              <div
                style={{ cursor: 'pointer', color: aggregates.status_distribution.PROD > 0 ? '#1890ff' : '#999' }}
                onClick={() => handleFilterClick('PROD')}
              >
                Production: {aggregates.status_distribution.PROD || 0} ({Math.round(((aggregates.status_distribution.PROD || 0) / aggregates.total_rules) * 100)}%)
              </div>
            </div>
          </Card>
        </Col>

        {/* Coverage Analysis Section */}
        <Col xs={24} lg={8}>
          <Card
            title={
              <span style={{ fontSize: '16px', fontWeight: 600 }}>
                COVERAGE ANALYSIS
              </span>
            }
            style={{ height: '220px' }}
          >
            <div style={{ fontSize: '14px', lineHeight: '1.8' }}>
              <div>
                Unique Actions: {aggregates.action_analysis.unique_actions}
              </div>
              <div>
                Referenced: {aggregates.action_analysis.unique_actions - aggregates.action_analysis.missing_actions}
              </div>
              <div
                style={{ cursor: 'pointer', color: aggregates.action_analysis.missing_actions > 0 ? '#ff4d4f' : '#52c41a' }}
                onClick={() => aggregates.action_analysis.missing_actions > 0 ? showMissingItems('actions') : null}
              >
                Missing Actions: {aggregates.action_analysis.missing_actions} ({Math.round((aggregates.action_analysis.missing_actions / aggregates.action_analysis.unique_actions) * 100)}%)
              </div>
              <div
                style={{
                  cursor: aggregates.attribute_analysis.missing_attributes > 0 ? 'pointer' : 'default',
                  color: aggregates.attribute_analysis.missing_attributes > 0 ? '#ff4d4f' : '#52c41a'
                }}
                onClick={() => aggregates.attribute_analysis.missing_attributes > 0 ? showMissingItems('attributes') : null}
              >
                Missing Attributes: {aggregates.attribute_analysis.missing_attributes}
              </div>
              <div>
                ActionSets Defined: {aggregates.type_distribution.actionset || 0}
              </div>
            </div>
          </Card>
        </Col>
      </Row>

      {/* Active Filters Display */}
      {activeFilters.length > 0 && (
        <div style={{ marginBottom: 16 }}>
          <Text strong>Active Filters: </Text>
          {activeFilters.map(filter => (
            <Tag
              key={filter}
              closable
              onClose={() => handleFilterClick(filter)}
              color="blue"
            >
              {filter.replace('_', ' ').toUpperCase()}
            </Tag>
          ))}
          <Button
            size="small"
            type="link"
            onClick={() => {
              setActiveFilters([]);
              setCurrentPage(1);
            }}
          >
            Clear All
          </Button>
        </div>
      )}


      {/* Results Table */}
      <Card>
        <Table
          columns={columns}
          dataSource={results}
          rowKey={record => `${record.rule.id}-${record.rule.name}`}
          pagination={false}
          scroll={{ x: 1200 }}
          expandable={{
            expandedRowRender: (record) => (
              <div style={{ padding: '16px 0' }}>
                <Row gutter={[16, 8]}>
                  <Col span={24}>
                    <Text strong>Rule Content:</Text>
                    <div style={{
                      background: '#f5f5f5',
                      padding: '12px',
                      borderRadius: '6px',
                      marginTop: '8px',
                      fontFamily: 'monospace',
                      fontSize: '12px'
                    }}>
                      {record.rule?.content || 'No content available'}
                    </div>
                  </Col>
                  {record.validation_errors && record.validation_errors.length > 0 && (
                    <Col span={24}>
                      <Text strong style={{ color: '#ff4d4f' }}>Validation Errors:</Text>
                      <ul style={{ color: '#ff4d4f', marginTop: '8px' }}>
                        {record.validation_errors.map((error, index) => (
                          <li key={index}>{error}</li>
                        ))}
                      </ul>
                    </Col>
                  )}
                  {record.missing_actions && record.missing_actions.length > 0 && (
                    <Col span={12}>
                      <Text strong style={{ color: '#fa8c16' }}>Missing Actions:</Text>
                      <div style={{ marginTop: '8px' }}>
                        {record.missing_actions.map(action => (
                          <Tag key={action} color="orange">{action}</Tag>
                        ))}
                      </div>
                    </Col>
                  )}
                  {record.extracted_attributes && record.extracted_attributes.length > 0 && (
                    <Col span={12}>
                      <Text strong>Extracted Attributes:</Text>
                      <div style={{ marginTop: '8px' }}>
                        {record.extracted_attributes.map(attr => (
                          <Tag key={attr} color="blue">{attr}</Tag>
                        ))}
                      </div>
                    </Col>
                  )}
                </Row>
              </div>
            ),
            rowExpandable: () => true,
          }}
        />

        {/* Pagination */}
        <div style={{ marginTop: 16, textAlign: 'center' }}>
          <Pagination
            current={currentPage}
            total={pagination.filtered}
            pageSize={pageSize}
            onChange={handlePageChange}
            showSizeChanger={false}
            showQuickJumper
            showTotal={(total, range) =>
              `${range[0]}-${range[1]} of ${total} items ${
                activeFilters.length > 0 ? '(filtered)' : ''
              }`
            }
          />
        </div>
      </Card>

      {/* Summary Cards */}
      {aggregates.action_analysis.most_referenced.length > 0 && (
        <Row gutter={[16, 16]} style={{ marginTop: 24 }}>
          <Col xs={24} md={12}>
            <Card title="Most Referenced Actions">
              {aggregates.action_analysis.most_referenced.map(action => (
                <div key={action.name} style={{ marginBottom: 8 }}>
                  <Text strong>{action.name}</Text>
                  <Text type="secondary" style={{ float: 'right' }}>
                    {action.count} references
                  </Text>
                </div>
              ))}
            </Card>
          </Col>
          <Col xs={24} md={12}>
            <Card title="Most Common Attributes">
              {aggregates.attribute_analysis.most_common_attributes.map(attr => (
                <div key={attr.name} style={{ marginBottom: 8 }}>
                  <Text strong>{attr.name}</Text>
                  <Text type="secondary" style={{ float: 'right' }}>
                    {attr.count} uses
                  </Text>
                </div>
              ))}
            </Card>
          </Col>
        </Row>
      )}

      {/* Missing Items Modal */}
      <Modal
        title={modalData.title}
        open={modalVisible}
        onCancel={() => setModalVisible(false)}
        footer={null}
        width={800}
      >
        <div style={{ marginBottom: 16 }}>
          <Typography.Text type="secondary">
            {modalData.type === 'actions'
              ? 'Actions/ActionSets referenced in rules but not defined in the rules table:'
              : 'Attributes used in rules but not defined in schema tables:'
            }
          </Typography.Text>
        </div>

        <Table
          dataSource={modalData.items.map((item, index) => ({ key: index, name: item }))}
          columns={[
            {
              title: modalData.type === 'actions' ? 'Missing Action/ActionSet' : 'Missing Attribute',
              dataIndex: 'name',
              key: 'name',
              render: (text) => (
                <Tag color={modalData.type === 'actions' ? 'red' : 'orange'}>
                  {text}
                </Tag>
              ),
            },
          ]}
          pagination={false}
          size="small"
          scroll={{ y: 400 }}
        />

        {modalData.items.length === 0 && (
          <Empty description="No missing items found" />
        )}
      </Modal>

      <style jsx>{`
        .filter-active {
          box-shadow: 0 2px 8px rgba(24, 144, 255, 0.3);
        }
      `}</style>
    </div>
  );
};

export default GapAnalysis;