import React, { useState, useEffect } from 'react';
import {
  Tree,
  Spin,
  Alert,
  Input,
  Card,
  Typography,
  Space,
  Tag,
  Breadcrumb,
} from 'antd';
import {
  AppstoreOutlined,
  SearchOutlined,
  HomeOutlined,
} from '@ant-design/icons';
import { rulesApi } from '../services/api';

const { Search } = Input;
const { Title } = Typography;

const RulesTreeNavigation = ({ onNodeSelect, selectedKeys = [], onBreadcrumbClick }) => {
  const [treeData, setTreeData] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const [expandedKeys, setExpandedKeys] = useState([]);
  const [searchValue, setSearchValue] = useState('');
  const [filteredTreeData, setFilteredTreeData] = useState([]);
  const [breadcrumb, setBreadcrumb] = useState([]);

  // Add synthetic nodes under process areas and get rule counts
  const addSyntheticNodes = async (nodes) => {
    const processedNodes = await Promise.all(nodes.map(async node => {
      if (node.type === 'process_area') {
        // Get counts for rules and actionsets in this process area
        try {
          const rulesResponse = await rulesApi.getRules({
            process_area_id: node.id,
            item_type: 'rule',
            limit: 1
          });
          const actionsetsResponse = await rulesApi.getRules({
            process_area_id: node.id,
            item_type: 'actionset',
            limit: 1
          });

          const rulesCount = rulesResponse.data.total || 0;
          const actionsetsCount = actionsetsResponse.data.total || 0;

          // Create synthetic nodes
          const syntheticChildren = [];

          if (rulesCount > 0) {
            syntheticChildren.push({
              key: `${node.key}-rules`,
              title: `📋 Rules (${rulesCount})`,
              type: 'synthetic_rules',
              parent_process_area_id: node.id,
              parent_process_area_name: node.name,
              isLeaf: true
            });
          }

          if (actionsetsCount > 0) {
            syntheticChildren.push({
              key: `${node.key}-actionsets`,
              title: `⚙️ ActionSets (${actionsetsCount})`,
              type: 'synthetic_actionsets',
              parent_process_area_id: node.id,
              parent_process_area_name: node.name,
              isLeaf: true
            });
          }

          return {
            ...node,
            children: syntheticChildren
          };
        } catch (error) {
          console.error('Error getting counts for process area:', node.id, error);
          // Fallback: show both synthetic nodes without counts
          return {
            ...node,
            children: [
              {
                key: `${node.key}-rules`,
                title: '📋 Rules',
                type: 'synthetic_rules',
                parent_process_area_id: node.id,
                parent_process_area_name: node.name,
                isLeaf: true
              },
              {
                key: `${node.key}-actionsets`,
                title: '⚙️ ActionSets',
                type: 'synthetic_actionsets',
                parent_process_area_id: node.id,
                parent_process_area_name: node.name,
                isLeaf: true
              }
            ]
          };
        }
      } else if (node.children) {
        // Recursively process children, but filter out individual rules
        const filteredChildren = node.children.filter(child => child.type !== 'rule');
        const processedChildren = await addSyntheticNodes(filteredChildren);
        return {
          ...node,
          children: processedChildren
        };
      } else {
        return node;
      }
    }));

    return processedNodes;
  };

  // Load tree data
  const loadTreeData = async () => {
    setLoading(true);
    setError(null);

    try {
      const response = await rulesApi.getHierarchyTree();
      const tree = response.data.tree;

      // Add synthetic nodes under process areas
      const treeWithSyntheticNodes = await addSyntheticNodes(tree);

      setTreeData(treeWithSyntheticNodes);
      setFilteredTreeData(treeWithSyntheticNodes);

      // Auto-expand first level by default
      const firstLevelKeys = treeWithSyntheticNodes.map(node => node.key);
      setExpandedKeys(firstLevelKeys);

    } catch (error) {
      setError('Failed to load navigation tree: ' + (error.response?.data?.error || error.message));
    } finally {
      setLoading(false);
    }
  };

  // Initial load
  useEffect(() => {
    loadTreeData();
  }, []);

  // Filter tree based on search
  const filterTreeData = (data, searchValue) => {
    if (!searchValue) return data;

    const filterNode = (node) => {
      // Skip individual rule nodes but keep synthetic nodes
      if (node.type === 'rule') return null;

      const childrenMatch = node.children?.map(filterNode).filter(Boolean) || [];
      const nodeMatch = node.title.toLowerCase().includes(searchValue.toLowerCase()) ||
                       node.name?.toLowerCase().includes(searchValue.toLowerCase());

      if (nodeMatch || childrenMatch.length > 0) {
        return {
          ...node,
          children: childrenMatch
        };
      }
      return null;
    };

    return data.map(filterNode).filter(Boolean);
  };

  // Handle search
  const handleSearch = (value) => {
    setSearchValue(value);
    const filtered = filterTreeData(treeData, value);
    setFilteredTreeData(filtered);
    
    // Auto-expand filtered nodes
    if (value) {
      const getAllKeys = (nodes) => {
        let keys = [];
        nodes.forEach(node => {
          keys.push(node.key);
          if (node.children) {
            keys = [...keys, ...getAllKeys(node.children)];
          }
        });
        return keys;
      };
      setExpandedKeys(getAllKeys(filtered));
    }
  };

  // Handle tree node selection
  const handleNodeSelect = async (selectedKeys, { node }) => {
    if (selectedKeys.length === 0) return;

    try {
      // Handle synthetic nodes differently
      if (node.type === 'synthetic_rules' || node.type === 'synthetic_actionsets') {
        // For synthetic nodes, load breadcrumb for the parent process area
        const processAreaId = node.parent_process_area_id;
        if (processAreaId) {
          const breadcrumbResponse = await rulesApi.getBreadcrumb('process_area', processAreaId);
          // Add synthetic node to breadcrumb
          const breadcrumbWithSynthetic = [
            ...breadcrumbResponse.data.breadcrumb,
            {
              type: node.type,
              name: node.title,
              id: node.key
            }
          ];
          setBreadcrumb(breadcrumbWithSynthetic);
        }
      } else {
        // Load breadcrumb for regular nodes
        const nodeType = node.type;
        const nodeId = node.id;

        if (nodeType && nodeId) {
          const breadcrumbResponse = await rulesApi.getBreadcrumb(nodeType, nodeId);
          setBreadcrumb(breadcrumbResponse.data.breadcrumb);
        }
      }

      // Call parent handler
      if (onNodeSelect) {
        onNodeSelect(selectedKeys, { node });
      }
    } catch (error) {
      console.error('Failed to load breadcrumb:', error);
    }
  };

  // Handle tree expansion
  const handleExpand = (expandedKeys) => {
    setExpandedKeys(expandedKeys);
  };



  // Render tree nodes without icons
  const renderTreeNodes = (nodes) => {
    return nodes.map(node => ({
      ...node,
      title: node.title,
      children: node.children ? renderTreeNodes(node.children) : undefined
    }));
  };

  // Handle breadcrumb click
  const handleBreadcrumbItemClick = (item) => {
    if (onBreadcrumbClick) {
      onBreadcrumbClick(item);
    }
  };

  if (loading) {
    return (
      <Card>
        <Spin tip="Loading navigation...">
          <div style={{ height: 200 }} />
        </Spin>
      </Card>
    );
  }

  if (error) {
    return (
      <Card>
        <Alert
          message="Navigation Error"
          description={error}
          type="error"
          showIcon
          action={
            <Space>
              <button onClick={loadTreeData}>Retry</button>
            </Space>
          }
        />
      </Card>
    );
  }

  return (
    <Card 
      size="small" 
      title={
        <Space>
          <AppstoreOutlined />
          <Title level={5} style={{ margin: 0 }}>Rules Navigation</Title>
        </Space>
      }
      style={{ height: '100%' }}
      bodyStyle={{ padding: 12, height: 'calc(100vh - 120px)', overflowY: 'auto' }}
    >
      {/* Search */}
      <Search
        placeholder="Search hierarchy..."
        allowClear
        value={searchValue}
        onChange={(e) => handleSearch(e.target.value)}
        style={{ marginBottom: 12 }}
        prefix={<SearchOutlined />}
      />

      {/* Breadcrumb */}
      {breadcrumb.length > 0 && (
        <Breadcrumb style={{ marginBottom: 12, fontSize: '12px' }}>
          <Breadcrumb.Item>
            <HomeOutlined />
          </Breadcrumb.Item>
          {breadcrumb.map((item, index) => (
            <Breadcrumb.Item 
              key={`${item.type}-${item.id}`}
              onClick={() => handleBreadcrumbItemClick(item)}
              style={{ cursor: 'pointer' }}
            >
              {item.code && item.name ? `${item.code} - ${item.name}` : item.name}
            </Breadcrumb.Item>
          ))}
        </Breadcrumb>
      )}

      {/* Tree */}
      <Tree
        showLine={{ showLeafIcon: false }}
        showIcon={false}
        expandedKeys={expandedKeys}
        selectedKeys={selectedKeys}
        onExpand={handleExpand}
        onSelect={handleNodeSelect}
        treeData={renderTreeNodes(filteredTreeData)}
        style={{ 
          background: 'transparent',
          fontSize: '13px'
        }}
      />

    </Card>
  );
};

export default RulesTreeNavigation;