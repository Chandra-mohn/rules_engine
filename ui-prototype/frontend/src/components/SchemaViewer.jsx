import React, { useState, useEffect } from 'react';
import {
  Modal,
  Tabs,
  Tag,
  Typography,
  Space,
  Input,
  Card,
  Collapse,
  List,
} from 'antd';
import {
  InfoCircleOutlined,
  FunctionOutlined,
  DatabaseOutlined,
  SettingOutlined,
} from '@ant-design/icons';
import { schemaApi, contextsApi } from '../services/api';

const { Text, Paragraph } = Typography;
const { Search } = Input;

const SchemaViewer = ({ visible, onClose, schemaVersion = 'modern', contextId = null }) => {
  const [schema, setSchema] = useState(null);
  const [contextData, setContextData] = useState(null);
  const [searchTerm, setSearchTerm] = useState('');
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    if (visible) {
      loadData();
    }
  }, [visible, schemaVersion, contextId]);

  const loadData = async () => {
    setLoading(true);
    try {
      // Load context data if contextId is provided
      if (contextId) {
        const response = await contextsApi.getContext(contextId);
        setContextData(response.data);
      } else {
        setContextData(null);
      }

      // Also load generic schema as fallback
      try {
        const response = await fetch(`/api/schema/${schemaVersion}`);
        const data = await response.json();
        setSchema(data);
      } catch (error) {
        console.error('Failed to load schema:', error);
        try {
          const response = await schemaApi.getFullSchema();
          setSchema(response.data);
        } catch (fallbackError) {
          console.error('Fallback schema load failed:', fallbackError);
        }
      }
    } catch (error) {
      console.error('Failed to load context:', error);
    } finally {
      setLoading(false);
    }
  };

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

  const renderContextDataTab = () => {
    if (!contextData || !contextData.context_data) {
      return (
        <Card>
          <Text type="secondary">
            No context selected. Select a context in the dropdown above to see available attributes.
          </Text>
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
      <div>
        <Card style={{ marginBottom: 16 }} size="small">
          <Space direction="vertical" style={{ width: '100%' }}>
            <Text strong>Context: {contextData.name}</Text>
            {contextData.description && <Text type="secondary">{contextData.description}</Text>}
          </Space>
        </Card>

        <Search
          placeholder="Search attributes..."
          onChange={(e) => setSearchTerm(e.target.value)}
          style={{ marginBottom: 16 }}
        />

        <Collapse
          defaultActiveKey={Object.keys(groupedAttributes)}
          items={filteredGroups.map(([entityName, attrs]) => {
            const leafAttrs = attrs.filter(a => !a.isParent);
            return {
              key: entityName,
              label: (
                <Space>
                  <DatabaseOutlined />
                  <strong>{entityName}</strong>
                  <Text type="secondary">({leafAttrs.length} attributes)</Text>
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
                      <List.Item>
                        <List.Item.Meta
                          title={
                            <Space>
                              <Text code copyable>{attr.path}</Text>
                              <Tag color="blue">{attr.type}</Tag>
                              <Text code>{displayValue}</Text>
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
      </div>
    );
  };

  const renderAttributesTab = () => {
    if (!schema) return null;

    return (
      <div>
        <Search
          placeholder="Search attributes..."
          onChange={(e) => setSearchTerm(e.target.value)}
          style={{ marginBottom: 16 }}
        />

        <Collapse
          defaultActiveKey={['applicant']}
          items={Object.entries(schema.attributes).map(([entityName, entityData]) => ({
            key: entityName,
            label: (
              <Space>
                <DatabaseOutlined />
                <strong>{entityName}</strong>
                <Text type="secondary">({Object.keys(entityData.properties || {}).length} attributes)</Text>
              </Space>
            ),
            children: (
              <div>
                <Paragraph type="secondary">{entityData.description}</Paragraph>

                <List
                  dataSource={Object.entries(entityData.properties || {}).filter(([propName, propData]) =>
                    !searchTerm ||
                    propName.toLowerCase().includes(searchTerm.toLowerCase()) ||
                    (propData.description || '').toLowerCase().includes(searchTerm.toLowerCase())
                  )}
                  renderItem={([propName, propData]) => (
                    <List.Item>
                      <List.Item.Meta
                        title={
                          <Space>
                            <Text code>{entityName}.{propName}</Text>
                            <Tag color="blue">{propData.type}</Tag>
                          </Space>
                        }
                        description={
                          <div>
                            <Paragraph>{propData.description}</Paragraph>
                            {propData.examples && (
                              <div>
                                <Text strong>Examples:</Text>
                                <ul style={{ marginTop: 4 }}>
                                  {propData.examples.map((example, idx) => (
                                    <li key={idx}>
                                      <Text code>{example}</Text>
                                    </li>
                                  ))}
                                </ul>
                              </div>
                            )}
                            {propData.range && (
                              <div>
                                <Text strong>Range:</Text> {propData.range[0]} - {propData.range[1] || '∞'}
                              </div>
                            )}
                            {propData.values && (
                              <div>
                                <Text strong>Values:</Text> {propData.values.join(', ')}
                              </div>
                            )}
                          </div>
                        }
                      />
                    </List.Item>
                  )}
                />
              </div>
            )
          }))}
        />
      </div>
    );
  };

  const renderActionsTab = () => {
    if (!schema) return null;

    const filteredActions = Object.entries(schema.actions).filter(([actionName, actionData]) =>
      !searchTerm || 
      actionName.toLowerCase().includes(searchTerm.toLowerCase()) ||
      actionData.description.toLowerCase().includes(searchTerm.toLowerCase())
    );

    return (
      <div>
        <Search
          placeholder="Search actions..."
          onChange={(e) => setSearchTerm(e.target.value)}
          style={{ marginBottom: 16 }}
        />
        
        <List
          dataSource={filteredActions}
          renderItem={([actionName, actionData]) => (
            <List.Item>
              <List.Item.Meta
                avatar={<FunctionOutlined style={{ color: '#1890ff' }} />}
                title={
                  <Space>
                    <Text code>{actionName}</Text>
                    <Tag color="green">{actionData.category}</Tag>
                  </Space>
                }
                description={
                  <div>
                    <Paragraph>{actionData.description}</Paragraph>
                    {actionData.examples && (
                      <div>
                        <Text strong>Examples:</Text>
                        <ul style={{ marginTop: 4 }}>
                          {actionData.examples.map((example, idx) => (
                            <li key={idx}>
                              <Text code>{example}</Text>
                            </li>
                          ))}
                        </ul>
                      </div>
                    )}
                  </div>
                }
              />
            </List.Item>
          )}
        />
      </div>
    );
  };

  const renderFunctionsTab = () => {
    if (!schema) return null;

    return (
      <div>
        <Search
          placeholder="Search functions..."
          onChange={(e) => setSearchTerm(e.target.value)}
          style={{ marginBottom: 16 }}
        />

        <Collapse
          items={Object.entries(schema.functions).map(([categoryName, categoryFunctions]) => ({
            key: categoryName,
            label: (
              <Space>
                <SettingOutlined />
                <strong>{categoryName.replace('_', ' ').toUpperCase()}</strong>
                <Text type="secondary">({Object.keys(categoryFunctions).length} functions)</Text>
              </Space>
            ),
            children: (
              <List
                dataSource={Object.entries(categoryFunctions).filter(([funcName, funcData]) =>
                  !searchTerm ||
                  funcName.toLowerCase().includes(searchTerm.toLowerCase()) ||
                  funcData.description.toLowerCase().includes(searchTerm.toLowerCase())
                )}
                renderItem={([funcName, funcData]) => (
                  <List.Item>
                    <List.Item.Meta
                      title={
                        <Space>
                          <Text code>{funcName}({funcData.parameters.join(', ')})</Text>
                          <Tag color="purple">{funcData.return_type}</Tag>
                        </Space>
                      }
                      description={
                        <div>
                          <Paragraph>{funcData.description}</Paragraph>
                          {funcData.examples && (
                            <div>
                              <Text strong>Examples:</Text>
                              <ul style={{ marginTop: 4 }}>
                                {funcData.examples.map((example, idx) => (
                                  <li key={idx}>
                                    <Text code>{example}</Text>
                                  </li>
                                ))}
                              </ul>
                            </div>
                          )}
                        </div>
                      }
                    />
                  </List.Item>
                )}
              />
            )
          }))}
        />
      </div>
    );
  };

  const renderKeywordsTab = () => {
    if (!schema) return null;

    return (
      <div>
        <Card title="Keywords" style={{ marginBottom: 16 }}>
          <Space wrap>
            {schema.keywords.map(keyword => (
              <Tag key={keyword} color="blue">{keyword}</Tag>
            ))}
          </Space>
        </Card>

        <Card title="Operators" style={{ marginBottom: 16 }}>
          <Space wrap>
            {schema.operators.map(operator => (
              <Tag key={operator} color="orange">{operator}</Tag>
            ))}
          </Space>
        </Card>

        <Card title="Time Units">
          <Space wrap>
            {schema.time_units.map(unit => (
              <Tag key={unit} color="green">{unit}</Tag>
            ))}
          </Space>
        </Card>
      </div>
    );
  };

  return (
    <Modal
      title={
        <Space>
          <InfoCircleOutlined />
          {contextData ? 'Context Data Reference' : 'Rules Schema Reference'}
        </Space>
      }
      open={visible}
      onCancel={onClose}
      footer={null}
      width={1000}
      style={{ top: 20 }}
      loading={loading}
    >
      <Tabs
        defaultActiveKey={contextData ? "context" : "attributes"}
        items={[
          ...(contextData ? [{
            key: 'context',
            label: 'Context Data',
            children: renderContextDataTab()
          }] : []),
          {
            key: 'attributes',
            label: 'Schema Attributes',
            children: renderAttributesTab()
          },
          {
            key: 'actions',
            label: 'Actions',
            children: renderActionsTab()
          },
          {
            key: 'functions',
            label: 'Functions',
            children: renderFunctionsTab()
          },
          {
            key: 'keywords',
            label: 'Keywords',
            children: renderKeywordsTab()
          }
        ]}
      />
    </Modal>
  );
};

export default SchemaViewer;