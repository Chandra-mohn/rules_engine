# 📋 Attributes and Actions Reference

## 🎯 **Where to Define Attributes and Actions**

### **Primary Configuration File**
```
📁 backend/config/rules_schema.py
```

This is the **single source of truth** for all attributes, actions, and functions in the Rules DSL.

---

## 🏗️ **Current Schema Overview**

### **📊 Available Attributes**

#### **Applicant Entity** (`applicant.*`)
- `creditScore` (number) - Credit score (300-850)
- `age` (number) - Age in years
- `annualIncome` (number) - Annual income in dollars
- `monthlyIncome` (number) - Monthly income in dollars
- `employmentStatus` (string) - Employment status
- `employmentYears` (number) - Years of employment
- `applicationDate` (date) - Application submission date
- `birthDate` (date) - Date of birth
- `requestedLimit` (number) - Requested credit limit
- `existingDebt` (number) - Total existing debt
- `bankruptcyHistory` (boolean) - Has bankruptcy history
- `ssn` (string) - Social Security Number

#### **Transaction Entity** (`transaction.*`)
- `amount` (number) - Transaction amount
- `timestamp` (datetime) - Transaction timestamp
- `merchantCategory` (string) - Merchant category code
- `location` (string) - Transaction location
- `type` (string) - Transaction type
- `isOnline` (boolean) - Is online transaction

#### **Account Entity** (`account.*`)
- `currentBalance` (number) - Current account balance
- `creditLimit` (number) - Credit limit
- `availableCredit` (number) - Available credit
- `paymentHistory` (string) - Payment history rating
- `accountAge` (number) - Account age in months

### **⚡ Available Actions**

#### **Approval Actions**
- `approveApplication` - Approve the credit card application
- `conditionalApproval` - Approve with conditions or lower limit
- `instantApproval` - Instant approval for qualified applicants

#### **Rejection Actions**
- `rejectApplication` - Reject the credit card application

#### **Review Actions**
- `manualReview` - Require manual review by underwriter
- `requireManualReview` - Flag for manual review
- `flagForReview` - Flag transaction for review

#### **Transaction Actions**
- `approveTransaction` - Approve the transaction
- `declineTransaction` - Decline the transaction

#### **Alert Actions**
- `sendAlert` - Send fraud alert

#### **Verification Actions**
- `requestVerification` - Request additional verification

#### **Limit Actions**
- `setLimit(amount)` - Set specific credit limit

### **🔧 Available Functions**

#### **Date/Time Functions**
- `now` - Current timestamp
- `today` - Current date at midnight
- `business_date` - Current business date (excludes weekends/holidays)
- `year_of(date)` - Extract year from date
- `month_of(date)` - Extract month from date
- `day_of(date)` - Extract day from date
- `day_of_week(date)` - Get day of week (1=Monday, 7=Sunday)
- `day_of_year(date)` - Get day of year (1-366)
- `week_of_year(date)` - Get week of year

#### **String Functions**
- `contains(string, substring)` - Check if string contains substring
- `starts_with(string, prefix)` - Check if string starts with prefix
- `ends_with(string, suffix)` - Check if string ends with suffix
- `matches(string, pattern)` - Check if string matches regex pattern

---

## 🚀 **How to Add New Items**

### **1. Adding New Attributes**

Edit `backend/config/rules_schema.py` and add to the appropriate entity:

```python
'applicant': {
    'properties': {
        # Add new attribute here
        'newAttribute': {
            'type': 'string',  # number, string, date, datetime, boolean
            'description': 'Description of the new attribute',
            'examples': ['applicant.newAttribute = "value"']
        }
    }
}
```

### **2. Adding New Actions**

Edit `backend/config/rules_schema.py` and add to `ACTIONS`:

```python
ACTIONS = {
    'newAction': {
        'description': 'Description of the new action',
        'category': 'approval',  # approval, rejection, review, transaction, alert, etc.
        'parameters': [],        # Add parameters if needed
        'examples': ['then newAction']
    }
}
```

### **3. Adding New Functions**

Edit `backend/config/rules_schema.py` and add to `FUNCTIONS`:

```python
FUNCTIONS = {
    'math': {  # or appropriate category
        'newFunction': {
            'description': 'Description of the new function',
            'return_type': 'number',  # number, string, boolean, date, datetime
            'parameters': ['param1'],
            'examples': ['newFunction(applicant.age) > 25']
        }
    }
}
```

---

## 🔄 **After Making Changes**

1. **Restart Backend Server**:
   ```bash
   ./scripts/start-backend.sh
   ```

2. **Test in Frontend**:
   - Open Rules Editor
   - Click "Schema Reference" button to see changes
   - Use Ctrl+Space for auto-completion
   - Create test rules with new attributes/actions

---

## 📍 **Where Schema is Used**

### **Backend Files**
- `backend/config/rules_schema.py` - **Main configuration**
- `backend/services/java_bridge.py` - Uses schema for auto-completion
- `backend/api/schema.py` - Exposes schema via REST API

### **Frontend Files**
- `frontend/src/components/SchemaViewer.jsx` - Displays schema reference
- `frontend/src/components/RuleEditor.jsx` - Uses schema for auto-completion
- `frontend/src/services/api.js` - API calls to get schema data

### **API Endpoints**
- `GET /api/schema` - Get complete schema
- `GET /api/schema/attributes` - Get all attributes
- `GET /api/schema/actions` - Get all actions
- `GET /api/schema/functions` - Get all functions

---

## 💡 **Example Usage in Rules**

```javascript
rule comprehensiveCheck:
    // Using attributes
    if applicant.creditScore >= 700 and 
       applicant.age >= 21 and
       applicant.annualIncome > 50000 and
       applicant.employmentYears >= 2
    then instantApproval
    
    // Using date functions
    if year_of(now) - year_of(applicant.birthDate) < 18
    then rejectApplication
    
    // Using business date
    if applicant.applicationDate after business_date + 30 days
    then conditionalApproval
    
    // Using transaction attributes
    if transaction.amount > 1000 and 
       day_of_week(transaction.timestamp) >= 6
    then manualReview
```

---

## 🎯 **Quick Reference**

| **What** | **Where to Define** | **How to Access** |
|----------|-------------------|------------------|
| **Attributes** | `rules_schema.py` → `ATTRIBUTES` | `applicant.creditScore` |
| **Actions** | `rules_schema.py` → `ACTIONS` | `then approveApplication` |
| **Functions** | `rules_schema.py` → `FUNCTIONS` | `year_of(applicant.birthDate)` |
| **Keywords** | `rules_schema.py` → `KEYWORDS` | `if`, `then`, `and`, `or` |
| **Operators** | `rules_schema.py` → `OPERATORS` | `>=`, `<`, `=`, `!=` |

**🔗 For detailed instructions, see:** `SCHEMA_CONFIGURATION.md`