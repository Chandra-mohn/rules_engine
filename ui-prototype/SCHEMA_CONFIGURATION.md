# Rules Schema Configuration Guide

This guide explains how to define and modify the attributes, actions, and functions available in the Rules DSL.

## 📁 Configuration Location

The schema is centrally defined in:
```
backend/config/rules_schema.py
```

## 🏗️ Schema Structure

### 1. Attributes (`ATTRIBUTES`)

Attributes represent data fields that can be used in rule conditions.

```python
ATTRIBUTES = {
    'entity_name': {
        'description': 'Entity description',
        'properties': {
            'property_name': {
                'type': 'data_type',           # number, string, date, datetime, boolean
                'description': 'Property description',
                'range': [min, max],           # Optional: for numeric types
                'values': ['val1', 'val2'],    # Optional: for enum types
                'examples': ['example usage']   # Optional: usage examples
            }
        }
    }
}
```

**Example - Adding a new applicant attribute:**
```python
'applicant': {
    'properties': {
        'phoneNumber': {
            'type': 'string',
            'description': 'Applicant phone number',
            'examples': ['applicant.phoneNumber matches "\\d{3}-\\d{3}-\\d{4}"']
        }
    }
}
```

### 2. Actions (`ACTIONS`)

Actions represent operations that can be executed when rule conditions are met.

```python
ACTIONS = {
    'action_name': {
        'description': 'Action description',
        'category': 'category_name',        # approval, rejection, review, etc.
        'parameters': ['param1', 'param2'], # Optional: action parameters
        'examples': ['then action_name']    # Usage examples
    }
}
```

**Example - Adding a new action:**
```python
'sendNotification': {
    'description': 'Send notification to applicant',
    'category': 'notification',
    'parameters': ['message'],
    'examples': ['then sendNotification("Application approved")']
}
```

### 3. Functions (`FUNCTIONS`)

Functions represent operations that can be used in rule expressions.

```python
FUNCTIONS = {
    'category_name': {
        'function_name': {
            'description': 'Function description',
            'return_type': 'return_data_type',  # number, string, boolean, date, datetime
            'parameters': ['param1', 'param2'], # Function parameters
            'examples': ['usage examples']      # Usage examples
        }
    }
}
```

**Example - Adding a new string function:**
```python
'string': {
    'length': {
        'description': 'Get string length',
        'return_type': 'number',
        'parameters': ['string'],
        'examples': ['length(applicant.name) > 5']
    }
}
```

## 🔧 How to Add New Schema Elements

### Adding New Attributes

1. **Open** `backend/config/rules_schema.py`
2. **Find** the appropriate entity in `ATTRIBUTES` (or create a new one)
3. **Add** the new property:

```python
'applicant': {
    'properties': {
        # ... existing properties ...
        'newProperty': {
            'type': 'string',
            'description': 'Description of the new property',
            'examples': ['applicant.newProperty = "value"']
        }
    }
}
```

### Adding New Actions

1. **Open** `backend/config/rules_schema.py`
2. **Add** to the `ACTIONS` dictionary:

```python
ACTIONS = {
    # ... existing actions ...
    'newAction': {
        'description': 'Description of the new action',
        'category': 'appropriate_category',
        'parameters': [],  # Add parameters if needed
        'examples': ['then newAction']
    }
}
```

### Adding New Functions

1. **Open** `backend/config/rules_schema.py`
2. **Add** to the appropriate category in `FUNCTIONS`:

```python
FUNCTIONS = {
    'math': {  # or create new category
        'newFunction': {
            'description': 'Description of the new function',
            'return_type': 'number',
            'parameters': ['param1'],
            'examples': ['newFunction(applicant.age) > 25']
        }
    }
}
```

### Adding New Entity Types

1. **Add** a new entity to `ATTRIBUTES`:

```python
ATTRIBUTES = {
    # ... existing entities ...
    'merchant': {
        'description': 'Merchant information',
        'properties': {
            'category': {
                'type': 'string',
                'description': 'Merchant category',
                'examples': ['merchant.category = "restaurant"']
            },
            'riskScore': {
                'type': 'number',
                'description': 'Merchant risk score',
                'range': [0, 100],
                'examples': ['merchant.riskScore < 50']
            }
        }
    }
}
```

## 🔄 After Making Changes

### 1. Restart the Backend Server
```bash
# Stop the current server (Ctrl+C)
# Restart
./scripts/start-backend.sh
```

### 2. Test the Changes
1. Open the Rules Editor in the frontend
2. Click "Schema Reference" to view your changes
3. Use auto-completion (Ctrl+Space) to see new attributes/actions
4. Create a test rule using the new schema elements

### 3. Validate Integration
- **Auto-completion**: New items should appear in Monaco Editor
- **Schema Viewer**: New items should be visible in the reference modal
- **Validation**: Rules using new schema should validate correctly

## 📋 Schema Categories

### Attribute Types
- `number` - Numeric values (integers, decimals)
- `string` - Text values
- `date` - Date values (YYYY-MM-DD)
- `datetime` - Date and time values
- `boolean` - True/false values

### Action Categories
- `approval` - Application approval actions
- `rejection` - Application rejection actions
- `review` - Manual review actions
- `transaction` - Transaction processing actions
- `alert` - Alert and notification actions
- `verification` - Verification request actions
- `limit` - Credit limit actions

### Function Categories
- `date_time` - Date and time functions
- `string` - String manipulation functions
- `math` - Mathematical functions
- `validation` - Validation functions

## 🎯 Best Practices

### 1. Naming Conventions
- **Attributes**: Use camelCase (`creditScore`, `annualIncome`)
- **Actions**: Use camelCase (`approveApplication`, `sendAlert`)
- **Functions**: Use snake_case (`year_of`, `day_of_week`)

### 2. Documentation
- Always provide clear descriptions
- Include practical examples
- Specify data types and ranges where applicable

### 3. Organization
- Group related attributes under logical entities
- Use consistent categories for actions
- Group functions by functionality

### 4. Validation
- Test new schema elements thoroughly
- Ensure examples work in actual rules
- Verify auto-completion works correctly

## 🔍 Example: Complete New Feature

Let's add support for merchant information:

```python
# 1. Add new entity to ATTRIBUTES
'merchant': {
    'description': 'Merchant information for transactions',
    'properties': {
        'name': {
            'type': 'string',
            'description': 'Merchant name',
            'examples': ['merchant.name contains "Walmart"']
        },
        'category': {
            'type': 'string',
            'description': 'Merchant category code',
            'examples': ['merchant.category = "5411"']
        },
        'riskLevel': {
            'type': 'string',
            'description': 'Merchant risk level',
            'values': ['low', 'medium', 'high'],
            'examples': ['merchant.riskLevel = "low"']
        }
    }
}

# 2. Add new action to ACTIONS
'blockMerchant': {
    'description': 'Block transactions from this merchant',
    'category': 'transaction',
    'parameters': [],
    'examples': ['then blockMerchant']
}

# 3. Add new function to FUNCTIONS
'string': {
    'merchant_category_name': {
        'description': 'Get merchant category name from code',
        'return_type': 'string',
        'parameters': ['category_code'],
        'examples': ['merchant_category_name(merchant.category) = "Grocery Store"']
    }
}
```

After adding these, you can create rules like:
```
rule merchantRiskCheck:
    if merchant.riskLevel = "high" then blockMerchant
    if merchant.category = "5411" and transaction.amount > 500 then manualReview
```

## 🚀 Advanced Configuration

For more advanced scenarios, you can:

1. **Add validation logic** in the Java bridge
2. **Create custom completion providers** for specific contexts
3. **Add parameter validation** for functions with parameters
4. **Implement dynamic schema loading** from external sources

The schema configuration system is designed to be flexible and extensible, allowing you to easily adapt the Rules DSL to your specific business requirements.