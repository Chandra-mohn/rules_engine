# VS Code Extension Backend Setup

## Overview

The Flask backend provides semantic validation for the Rules DSL VS Code extension.

## Port Configuration

- **Main UI Backend**: Port 5001 (for React frontend)
- **Extension Backend**: Port 5002 (for VS Code extension)

## Starting the Extension Backend

### Option 1: Using the startup script
```bash
cd backend
./start_extension_backend.sh
```

### Option 2: Using Python directly
```bash
cd backend
python app.py --extension
```

### Option 3: Default port (5001)
```bash
cd backend
python app.py
```

## Semantic Validation Endpoint

**Endpoint**: `POST /api/validate/semantic`

**Request**:
```json
{
  "content": "rule \"My Rule\":\n    if applicant.creditScore > 700 then\n        approveTransaction()\n    endif\n"
}
```

**Response**:
```json
{
  "valid": true,
  "errors": [],
  "warnings": []
}
```

**Error Format**:
```json
{
  "line": 5,
  "column": 9,
  "severity": "error",
  "message": "Unknown action: 'unknownAction'",
  "suggestion": "Did you mean: approveTransaction, declineTransaction?",
  "type": "undefined_action"
}
```

## Validation Features

1. **Undefined Entity Fields**
   - Checks attributes against entity schemas
   - Provides "did you mean" suggestions using Levenshtein distance

2. **Missing Actions/ActionSets**
   - Validates action calls against defined primitive actions and actionsets
   - Suggests similar action names

3. **Cyclic ActionSet Calls**
   - Detects circular dependencies in actionset calls
   - Shows the complete cycle path

4. **Type Checking**
   - Validates data types in comparisons
   - Checks for type mismatches

## Testing

Run the test suite:
```bash
cd backend
python test_semantic_validation.py
```

## VS Code Extension Configuration

In VS Code settings, ensure:
```json
{
  "rules.backend.url": "http://localhost:5002"
}
```

## Keyboard Shortcut

In VS Code:
- **Alt+R B**: Run semantic validation (Build)

## Troubleshooting

### Backend not running
```bash
# Check if port 5002 is in use
lsof -ti:5002

# Kill existing process
kill $(lsof -ti:5002)

# Restart backend
python app.py --extension
```

### Connection errors in VS Code
1. Verify backend is running: `curl http://localhost:5002/api/health`
2. Check VS Code setting: `rules.backend.url`
3. Check extension output channel: "Rules DSL Build"

### Schema/Action not found
- Schemas are loaded from: `backend/test_data/schemas/`
- Actions are defined in: `backend/services/cycle_detector.py` (PRIMITIVE_ACTIONS)
- ActionSets are loaded from rule files with `item_type: 'actionset'`
