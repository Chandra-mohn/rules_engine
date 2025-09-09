import React, { useState, useEffect } from 'react';
import { Card, Button, Typography, Space, Tag, Divider, Alert } from 'antd';
import { ReloadOutlined, DeleteOutlined } from '@ant-design/icons';
import suggestionCache from '../services/suggestionCache';

const { Text, Paragraph } = Typography;

const CacheDebugger = () => {
  const [stats, setStats] = useState(null);
  const [refreshing, setRefreshing] = useState(false);

  const updateStats = () => {
    setStats(suggestionCache.getStats());
  };

  useEffect(() => {
    updateStats();
    
    // Update stats every 5 seconds
    const interval = setInterval(updateStats, 5000);
    return () => clearInterval(interval);
  }, []);

  const handleRefresh = async () => {
    setRefreshing(true);
    try {
      await suggestionCache.refresh();
      updateStats();
    } catch (error) {
      console.error('Failed to refresh cache:', error);
    } finally {
      setRefreshing(false);
    }
  };

  const handleClear = () => {
    suggestionCache.clear();
    updateStats();
  };

  if (!stats) return null;

  return (
    <Card 
      title="Suggestions Cache Debug" 
      size="small"
      extra={
        <Space>
          <Button 
            size="small" 
            icon={<ReloadOutlined />} 
            onClick={handleRefresh}
            loading={refreshing}
          >
            Refresh
          </Button>
          <Button 
            size="small" 
            icon={<DeleteOutlined />} 
            onClick={handleClear}
            danger
          >
            Clear
          </Button>
        </Space>
      }
    >
      {!stats.loaded ? (
        <Alert 
          type="warning" 
          message="Cache not loaded" 
          description={stats.error || "Use 'Refresh' to load the cache"} 
        />
      ) : (
        <>
          <Space direction="vertical" style={{ width: '100%' }}>
            <div>
              <Text strong>Status: </Text>
              <Tag color="green">Loaded</Tag>
              <Text type="secondary">v{stats.version}</Text>
            </div>
            
            <div>
              <Text strong>Last Updated: </Text>
              <Text>{new Date(stats.timestamp).toLocaleTimeString()}</Text>
            </div>

            <div>
              <Text strong>Total Items: </Text>
              <Tag color="blue">{stats.totalCount.toLocaleString()}</Tag>
            </div>

            <div>
              <Text strong>Memory Usage: </Text>
              <Tag color="purple">{stats.memorySizeKB} KB</Tag>
            </div>

            <Divider style={{ margin: '8px 0' }} />

            <div>
              <Text strong>Categories:</Text>
              <div style={{ marginTop: 4 }}>
                <Space wrap>
                  <Tag>Attributes: {stats.categories.attributes}</Tag>
                  <Tag>Actions: {stats.categories.actions}</Tag>
                  <Tag>Functions: {stats.categories.functions}</Tag>
                  <Tag>Keywords: {stats.categories.keywords}</Tag>
                  <Tag>Operators: {stats.categories.operators}</Tag>
                </Space>
              </div>
            </div>
          </Space>
        </>
      )}
    </Card>
  );
};

export default CacheDebugger;