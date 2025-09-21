import React, { useState } from 'react';
import { Layout } from 'antd';
import RulesListEnhanced from './components/RulesListEnhanced';
import RuleEditor from './components/RuleEditor';
import './App.css';

const { Header, Content } = Layout;

function App() {
  const [currentView, setCurrentView] = useState('list'); // 'list' or 'editor'
  const [selectedRule, setSelectedRule] = useState(null);

  // Handle create new rule
  const handleCreateRule = (ruleType = 'rule') => {
    setSelectedRule({ item_type: ruleType });
    setCurrentView('editor');
  };

  // Handle edit existing rule
  const handleEditRule = (rule) => {
    setSelectedRule(rule);
    setCurrentView('editor');
  };

  // Handle back to list
  const handleBackToList = () => {
    setSelectedRule(null);
    setCurrentView('list');
  };

  // Handle save rule
  const handleSaveRule = (savedRule) => {
    // Optionally stay in editor or go back to list
    // For now, let's stay in editor to show the updated rule
    setSelectedRule(savedRule);
    
    // Could also go back to list:
    // handleBackToList();
  };

  return (
    <Layout className="app-layout">
      <Header className="app-header">
        <div className="app-title">
          <h1>Rules Authoring System</h1>
          <span className="app-subtitle">Credit Card Processing Rules Engine</span>
        </div>
      </Header>
      
      <Content className="app-content">
        <div className="content-wrapper">
          {currentView === 'list' ? (
            <RulesListEnhanced
              onCreateRule={handleCreateRule}
              onEditRule={handleEditRule}
            />
          ) : (
            <RuleEditor
              rule={selectedRule}
              onBack={handleBackToList}
              onSave={handleSaveRule}
            />
          )}
        </div>
      </Content>
    </Layout>
  );
}

export default App;