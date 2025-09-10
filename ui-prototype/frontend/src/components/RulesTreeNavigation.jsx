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

  // Remove rules from tree data, stop at process area level
  const removeRulesFromTree = (nodes) => {
    return nodes.map(node => ({
      ...node,
      children: node.children ? node.children.filter(child => child.type !== 'rule').map(child => removeRulesFromTree([child])[0]) : []
    }));
  };

  // Load tree data
  const loadTreeData = async () => {
    setLoading(true);
    setError(null);
    
    try {
      const response = await rulesApi.getHierarchyTree();
      const tree = response.data.tree;
      
      // Remove rules from tree, stop at process area level
      const treeWithoutRules = removeRulesFromTree(tree);
      
      setTreeData(treeWithoutRules);
      setFilteredTreeData(treeWithoutRules);
      
      // Auto-expand first level by default
      const firstLevelKeys = treeWithoutRules.map(node => node.key);
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
      // Skip rule nodes
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
      // Load breadcrumb for selected node
      const nodeType = node.type;
      const nodeId = node.id;
      
      if (nodeType && nodeId) {
        const breadcrumbResponse = await rulesApi.getBreadcrumb(nodeType, nodeId);
        setBreadcrumb(breadcrumbResponse.data.breadcrumb);
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
        placeholder="Search rules..."
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