import React, { useState, useEffect } from 'react';
import {
  Card,
  List,
  Typography,
  Space,
  Tag,
  Input,
  Collapse,
  Spin,
  Empty,
} from 'antd';
import {
  DatabaseOutlined,
  CloseOutlined,
} from '@ant-design/icons';
import { contextsApi } from '../services/api';

const { Text } = Typography;
const { Search } = Input;

const ContextPanel = ({ contextId, onClose }) => {
  const [contextData, setContextData] = useState(null);
  const [searchTerm, setSearchTerm] = useState('');
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    const loadContextData = async () => {
      setLoading(true);
      try {
        const response = await contextsApi.getContext(contextId);
        setContextData(response.data);
      } catch (error) {
        console.error('Failed to load context:', error);
      } finally {
        setLoading(false);
      }
    };

    if (contextId) {
      loadContextData();
    }
  }, [contextId]);

  // Extract all attribute paths from context JSON
  const extractAttributePaths = (obj, prefix = '') => {
    const paths = [];

    if (typeof obj !== 'object' || obj === null) {
      return paths;
    }

    Object.entries(obj).forEach(([key, value]) => {
      const currentPath = prefix ? `${prefix}.${key}` : key;

      // Skip metadata fields
      if (key === '_metadata') {
        return;
      }

      if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
        // It's a nested object - add the parent and recurse
        paths.push({
          path: currentPath,
          type: 'object',
          value: `{...}`,
          isParent: true
        });
        paths.push(...extractAttributePaths(value, currentPath));
      } else {
        // It's a leaf value
        paths.push({
          path: currentPath,
          type: Array.isArray(value) ? 'array' : typeof value,
          value: Array.isArray(value) ? `[${value.length} items]` : String(value),
          isParent: false
        });
      }
    });

    return paths;
  };

  if (!contextId) {
    return (
      <Card
        size="small"
        style={{ height: '100%', overflow: 'auto' }}
        title="Context Data"
      >
        <Empty
          description="No context selected"
          image={Empty.PRESENTED_IMAGE_SIMPLE}
        />
      </Card>
    );
  }

  if (loading) {
    return (
      <Card size="small" style={{ height: '100%' }}>
        <div style={{ display: 'flex', justifyContent: 'center', alignItems: 'center', minHeight: 200 }}>
          <Spin tip="Loading context...">
            <div style={{ padding: 50 }} />
          </Spin>
        </div>
      </Card>
    );
  }

  if (!contextData || !contextData.context_data) {
    return (
      <Card size="small" style={{ height: '100%' }}>
        <Empty description="No context data available" />
      </Card>
    );
  }

  const contextJson = typeof contextData.context_data === 'string'
    ? JSON.parse(contextData.context_data)
    : contextData.context_data;

  const attributes = extractAttributePaths(contextJson);

  // Group by top-level entity
  const groupedAttributes = {};
  attributes.forEach(attr => {
    const topLevel = attr.path.split('.')[0];
    if (!groupedAttributes[topLevel]) {
      groupedAttributes[topLevel] = [];
    }
    groupedAttributes[topLevel].push(attr);
  });

  const filteredGroups = Object.entries(groupedAttributes).filter(([entity, attrs]) => {
    if (!searchTerm) return true;
    return entity.toLowerCase().includes(searchTerm.toLowerCase()) ||
      attrs.some(attr =>
        attr.path.toLowerCase().includes(searchTerm.toLowerCase()) ||
        String(attr.value).toLowerCase().includes(searchTerm.toLowerCase())
      );
  });

  return (
    <Card
      size="small"
      style={{ height: '100%', overflow: 'auto' }}
      title={
        <Space style={{ width: '100%', justifyContent: 'space-between' }}>
          <Text strong>Context Data</Text>
          {onClose && (
            <CloseOutlined
              onClick={onClose}
              style={{ cursor: 'pointer', fontSize: '12px' }}
            />
          )}
        </Space>
      }
    >
      <Space direction="vertical" style={{ width: '100%', marginBottom: 12 }}>
        <Text strong>{contextData.name}</Text>
        {contextData.description && (
          <Text type="secondary" style={{ fontSize: '12px' }}>
            {contextData.description}
          </Text>
        )}
      </Space>

      <Search
        placeholder="Search attributes..."
        size="small"
        onChange={(e) => setSearchTerm(e.target.value)}
        style={{ marginBottom: 12 }}
      />

      <Collapse
        size="small"
        defaultActiveKey={Object.keys(groupedAttributes)}
        items={filteredGroups.map(([entityName, attrs]) => {
          const leafAttrs = attrs.filter(a => !a.isParent);
          return {
            key: entityName,
            label: (
              <Space size="small">
                <DatabaseOutlined style={{ fontSize: '12px' }} />
                <Text strong style={{ fontSize: '12px' }}>{entityName}</Text>
                <Text type="secondary" style={{ fontSize: '11px' }}>
                  ({leafAttrs.length})
                </Text>
              </Space>
            ),
            children: (
              <List
                size="small"
                dataSource={leafAttrs.filter(attr =>
                  !searchTerm ||
                  attr.path.toLowerCase().includes(searchTerm.toLowerCase()) ||
                  String(attr.value).toLowerCase().includes(searchTerm.toLowerCase())
                )}
                renderItem={(attr) => {
                  // Truncate long values (>50 chars)
                  const displayValue = String(attr.value).length > 50
                    ? String(attr.value).substring(0, 50) + '...'
                    : attr.value;

                  return (
                    <List.Item style={{ padding: '4px 0' }}>
                      <List.Item.Meta
                        title={
                          <Space size="small" style={{ fontSize: '12px' }}>
                            <Text code copyable style={{ fontSize: '11px' }}>
                              {attr.path}
                            </Text>
                            <Tag color="blue" style={{ fontSize: '10px', margin: 0 }}>
                              {attr.type}
                            </Tag>
                            <Text code style={{ fontSize: '11px' }}>
                              {displayValue}
                            </Text>
                          </Space>
                        }
                      />
                    </List.Item>
                  );
                }}
              />
            )
          };
        })}
      />
    </Card>
  );
};

export default ContextPanel;
