import React, { useState, useEffect } from 'react';
import {
  Modal,
  Tabs,
  Button,
  Space,
  Card,
  message,
  Tooltip,
  Divider
} from 'antd';
import {
  ArrowLeftOutlined,
  ReloadOutlined,
  SaveOutlined,
  ExperimentOutlined,
  ImportOutlined,
  ExportOutlined,
  UserOutlined,
  CreditCardOutlined,
  BankOutlined
} from '@ant-design/icons';
import Editor from '@monaco-editor/react';

const { TabPane } = Tabs;

const SampleDataEditor = ({ visible, onClose, onTest, currentRuleContent }) => {
  const [activeTab, setActiveTab] = useState('applicant');
  const [applicantData, setApplicantData] = useState('');
  const [transactionData, setTransactionData] = useState('');
  const [accountData, setAccountData] = useState('');
  const [loading, setLoading] = useState(false);

  // Default sample data (aligned with existing rules)
  const defaultApplicantData = {
    creditScore: 750,  // > 700 to trigger approveApplication
    age: 28,           // >= 18 to trigger approveApplication  
    annualIncome: 75000,
    monthlyIncome: 6250,
    employmentStatus: "employed",
    employmentYears: 3,
    applicationDate: "2024-01-15",  // Aligned with business_date
    birthDate: "1995-03-22",
    requestedLimit: 5000,
    existingDebt: 12000,
    bankruptcyHistory: false,
    ssn: "123-45-6789"
  };

  const defaultTransactionData = {
    amount: 150.00,
    timestamp: "2024-01-15T14:30:00Z",
    merchantCategory: "5411",
    location: "US-CA-San Francisco",
    type: "purchase",
    isOnline: false
  };

  const defaultAccountData = {
    currentBalance: 1250.00,
    creditLimit: 5000,
    availableCredit: 3750.00,
    paymentHistory: "excellent",
    accountAge: 24
  };

  // Predefined templates (aligned with rules for testing)
  const applicantTemplates = {
    '✅ High Credit + Adult': { ...defaultApplicantData, creditScore: 800, age: 25, annualIncome: 120000 },
    '❌ Poor Credit + Adult': { ...defaultApplicantData, creditScore: 550, age: 25, annualIncome: 45000 },
    '❌ Good Credit + Minor': { ...defaultApplicantData, creditScore: 750, age: 17, birthDate: "2006-05-10" },
    '✅ Border Credit + Adult': { ...defaultApplicantData, creditScore: 700, age: 18, annualIncome: 45000 },
    '❌ Double Rejection': { ...defaultApplicantData, creditScore: 520, age: 17, bankruptcyHistory: true },
    '✅ Premium Profile': { ...defaultApplicantData, creditScore: 820, age: 35, annualIncome: 200000 },
    '⚠️ Mixed Signals': { ...defaultApplicantData, creditScore: 680, age: 19, annualIncome: 35000 },
    '✅ Just Qualified': { ...defaultApplicantData, creditScore: 701, age: 18, annualIncome: 30000 }
  };

  const transactionTemplates = {
    'Small Purchase': { ...defaultTransactionData, amount: 25.50 },
    'Large Purchase': { ...defaultTransactionData, amount: 2500.00 },
    'Online Purchase': { ...defaultTransactionData, isOnline: true, location: "US-NY-New York" },
    'Cash Advance': { ...defaultTransactionData, type: "cash_advance", amount: 500.00 },
    'International': { ...defaultTransactionData, location: "GB-London", amount: 180.00 },
    'High Risk Merchant': { ...defaultTransactionData, merchantCategory: "7995", amount: 300.00 }
  };

  const accountTemplates = {
    'New Account': { ...defaultAccountData, accountAge: 2, creditLimit: 1000, currentBalance: 0 },
    'Maxed Out': { ...defaultAccountData, currentBalance: 4900, availableCredit: 100 },
    'Premium Account': { ...defaultAccountData, creditLimit: 25000, paymentHistory: "excellent", accountAge: 60 },
    'Poor History': { ...defaultAccountData, paymentHistory: "poor", currentBalance: 2800 }
  };

  // Initialize data on mount
  useEffect(() => {
    if (visible) {
      resetToDefaults();
    }
  }, [visible]);

  const resetToDefaults = () => {
    setApplicantData(JSON.stringify(defaultApplicantData, null, 2));
    setTransactionData(JSON.stringify(defaultTransactionData, null, 2));
    setAccountData(JSON.stringify(defaultAccountData, null, 2));
  };

  const resetCurrentTab = () => {
    switch (activeTab) {
      case 'applicant':
        setApplicantData(JSON.stringify(defaultApplicantData, null, 2));
        break;
      case 'transaction':
        setTransactionData(JSON.stringify(defaultTransactionData, null, 2));
        break;
      case 'account':
        setAccountData(JSON.stringify(defaultAccountData, null, 2));
        break;
    }
    message.success('Reset to default values');
  };

  const applyTemplate = (template, category) => {
    const jsonString = JSON.stringify(template, null, 2);
    switch (category) {
      case 'applicant':
        setApplicantData(jsonString);
        break;
      case 'transaction':
        setTransactionData(jsonString);
        break;
      case 'account':
        setAccountData(jsonString);
        break;
    }
    message.success('Template applied');
  };

  const validateJSON = (jsonString) => {
    try {
      JSON.parse(jsonString);
      return true;
    } catch (error) {
      return false;
    }
  };

  const getCombinedTestData = () => {
    try {
      const applicant = JSON.parse(applicantData);
      const transaction = JSON.parse(transactionData);
      const account = JSON.parse(accountData);

      return {
        applicant,
        transaction,
        account,
        metadata: {
          business_date: new Date().toISOString().split('T')[0],
          test_timestamp: new Date().toISOString()
        }
      };
    } catch (error) {
      throw new Error('Invalid JSON in one or more data sections');
    }
  };

  const handleSave = () => {
    if (!validateJSON(applicantData) || !validateJSON(transactionData) || !validateJSON(accountData)) {
      message.error('Please fix JSON syntax errors before saving');
      return;
    }

    // Save to localStorage for persistence
    localStorage.setItem('sampleTestData', JSON.stringify({
      applicant: applicantData,
      transaction: transactionData,
      account: accountData
    }));

    message.success('Sample data saved');
  };

  const handleTestWithCurrentRule = async () => {
    if (!currentRuleContent) {
      message.error('No rule content available for testing');
      return;
    }

    try {
      const testData = getCombinedTestData();
      setLoading(true);
      
      if (onTest) {
        await onTest(testData);
      }
    } catch (error) {
      message.error(error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleExport = () => {
    try {
      const combinedData = getCombinedTestData();
      const dataStr = JSON.stringify(combinedData, null, 2);
      const dataBlob = new Blob([dataStr], { type: 'application/json' });
      
      const link = document.createElement('a');
      link.href = URL.createObjectURL(dataBlob);
      link.download = 'sample-test-data.json';
      link.click();
      
      message.success('Test data exported');
    } catch (error) {
      message.error('Failed to export: ' + error.message);
    }
  };

  const handleImport = () => {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = '.json';
    
    input.onchange = (e) => {
      const file = e.target.files[0];
      if (!file) return;
      
      const reader = new FileReader();
      reader.onload = (e) => {
        try {
          const data = JSON.parse(e.target.result);
          
          if (data.applicant) setApplicantData(JSON.stringify(data.applicant, null, 2));
          if (data.transaction) setTransactionData(JSON.stringify(data.transaction, null, 2));
          if (data.account) setAccountData(JSON.stringify(data.account, null, 2));
          
          message.success('Test data imported');
        } catch (error) {
          message.error('Invalid JSON file');
        }
      };
      reader.readAsText(file);
    };
    
    input.click();
  };

  const getCurrentTemplates = () => {
    switch (activeTab) {
      case 'applicant': return applicantTemplates;
      case 'transaction': return transactionTemplates;
      case 'account': return accountTemplates;
      default: return {};
    }
  };

  const getCurrentData = () => {
    switch (activeTab) {
      case 'applicant': return applicantData;
      case 'transaction': return transactionData;
      case 'account': return accountData;
      default: return '';
    }
  };

  const setCurrentData = (value) => {
    switch (activeTab) {
      case 'applicant': setApplicantData(value); break;
      case 'transaction': setTransactionData(value); break;
      case 'account': setAccountData(value); break;
    }
  };

  return (
    <Modal
      title={
        <div style={{ display: 'flex', alignItems: 'center', gap: '12px' }}>
          <Button 
            type="text" 
            icon={<ArrowLeftOutlined />} 
            onClick={onClose}
          />
          <span>Sample Test Data</span>
        </div>
      }
      visible={visible}
      onCancel={onClose}
      width={1200}
      footer={null}
      destroyOnClose
    >
      <div style={{ marginBottom: '16px' }}>
        <p style={{ color: '#666', margin: 0 }}>
          Configure the JSON data structure used for testing rules
        </p>
      </div>

      <Tabs 
        activeKey={activeTab} 
        onChange={setActiveTab}
        size="large"
      >
        <TabPane 
          tab={
            <span>
              <UserOutlined />
              Applicant Data
            </span>
          } 
          key="applicant" 
        />
        <TabPane 
          tab={
            <span>
              <CreditCardOutlined />
              Transaction Data
            </span>
          } 
          key="transaction" 
        />
        <TabPane 
          tab={
            <span>
              <BankOutlined />
              Account Data
            </span>
          } 
          key="account" 
        />
      </Tabs>

      <div style={{ marginBottom: '16px' }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '12px' }}>
          <h4 style={{ margin: 0 }}>{activeTab.charAt(0).toUpperCase() + activeTab.slice(1)} Data</h4>
          <Button 
            size="small" 
            icon={<ReloadOutlined />} 
            onClick={resetCurrentTab}
          >
            Reset
          </Button>
        </div>

        <div style={{ border: '1px solid #d9d9d9', borderRadius: '6px', overflow: 'hidden' }}>
          <Editor
            height="300px"
            language="json"
            theme="light"
            value={getCurrentData()}
            onChange={setCurrentData}
            options={{
              minimap: { enabled: false },
              scrollBeyondLastLine: false,
              fontSize: 13,
              lineNumbers: 'on',
              roundedSelection: false,
              automaticLayout: true
            }}
          />
        </div>
      </div>

      <Card 
        title="Predefined Templates" 
        size="small" 
        style={{ marginBottom: '16px' }}
      >
        <Space wrap>
          {Object.entries(getCurrentTemplates()).map(([name, template]) => (
            <Button 
              key={name}
              size="small"
              onClick={() => applyTemplate(template, activeTab)}
            >
              {name}
            </Button>
          ))}
        </Space>
      </Card>

      <Card title="Combined Test Data Preview" size="small" style={{ marginBottom: '16px' }}>
        <div style={{ 
          background: '#f5f5f5', 
          padding: '12px', 
          borderRadius: '4px', 
          fontFamily: 'monospace',
          fontSize: '12px',
          maxHeight: '200px',
          overflow: 'auto'
        }}>
          <pre>
            {(() => {
              try {
                return JSON.stringify(getCombinedTestData(), null, 2);
              } catch (error) {
                return '// Invalid JSON - please fix syntax errors in the data sections above';
              }
            })()}
          </pre>
        </div>
      </Card>

      <Divider />

      <div style={{ display: 'flex', justifyContent: 'space-between' }}>
        <Space>
          <Button 
            type="primary"
            icon={<SaveOutlined />}
            onClick={handleSave}
          >
            Save Changes
          </Button>
          <Button 
            icon={<ExperimentOutlined />}
            onClick={handleTestWithCurrentRule}
            loading={loading}
            disabled={!currentRuleContent}
          >
            Test with Current Rule
          </Button>
        </Space>

        <Space>
          <Button 
            icon={<ImportOutlined />}
            onClick={handleImport}
          >
            Import JSON
          </Button>
          <Button 
            icon={<ExportOutlined />}
            onClick={handleExport}
          >
            Export
          </Button>
        </Space>
      </div>
    </Modal>
  );
};

export default SampleDataEditor;