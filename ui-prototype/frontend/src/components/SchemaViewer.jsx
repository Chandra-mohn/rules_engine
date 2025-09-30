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
import { schemaApi } from '../services/api';

const { Text, Paragraph } = Typography;
const { TabPane } = Tabs;
const { Search } = Input;
const { Panel } = Collapse;

const SchemaViewer = ({ visible, onClose, schemaVersion = 'modern' }) => {
  const [schema, setSchema] = useState(null);
  const [searchTerm, setSearchTerm] = useState('');

  useEffect(() => {
    if (visible) {
      loadSchema();
    }
  }, [visible, schemaVersion]);

  const loadSchema = async () => {
    try {
      const response = await fetch(`/api/schema/${schemaVersion}`);
      const data = await response.json();
      setSchema(data);
    } catch (error) {
      console.error('Failed to load schema:', error);
      // Fallback to old API
      try {
        const response = await schemaApi.getFullSchema();
        setSchema(response.data);
      } catch (fallbackError) {
        console.error('Fallback schema load failed:', fallbackError);
      }
    }
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
        
        <Collapse defaultActiveKey={['applicant']}>
          {Object.entries(schema.attributes).map(([entityName, entityData]) => (
            <Panel
              key={entityName}
              header={
                <Space>
                  <DatabaseOutlined />
                  <strong>{entityName}</strong>
                  <Text type="secondary">({Object.keys(entityData.properties || {}).length} attributes)</Text>
                </Space>
              }
            >
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
            </Panel>
          ))}
        </Collapse>
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
        
        <Collapse>
          {Object.entries(schema.functions).map(([categoryName, categoryFunctions]) => (
            <Panel
              key={categoryName}
              header={
                <Space>
                  <SettingOutlined />
                  <strong>{categoryName.replace('_', ' ').toUpperCase()}</strong>
                  <Text type="secondary">({Object.keys(categoryFunctions).length} functions)</Text>
                </Space>
              }
            >
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
            </Panel>
          ))}
        </Collapse>
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
          Rules Schema Reference
        </Space>
      }
      open={visible}
      onCancel={onClose}
      footer={null}
      width={1000}
      style={{ top: 20 }}
    >
      <Tabs defaultActiveKey="attributes">
        <TabPane tab="Attributes" key="attributes">
          {renderAttributesTab()}
        </TabPane>
        <TabPane tab="Actions" key="actions">
          {renderActionsTab()}
        </TabPane>
        <TabPane tab="Functions" key="functions">
          {renderFunctionsTab()}
        </TabPane>
        <TabPane tab="Keywords" key="keywords">
          {renderKeywordsTab()}
        </TabPane>
      </Tabs>
    </Modal>
  );
};

export default SchemaViewer;