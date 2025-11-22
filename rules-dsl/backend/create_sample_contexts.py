#!/usr/bin/env python3
"""
Create sample contexts to demonstrate the context system.
Creates both a schema template and test contexts.
"""

import requests
import json

API_BASE = "http://localhost:5001/api"

# Schema Template Context - Defines all available attributes
schema_template = {
    "name": "Master Schema - Credit Card v1.0",
    "description": "Complete schema definition for credit card processing rules",
    "is_schema_template": True,
    "version": "1.0.0",
    "context_data": {
        "applicant": {
            "creditScore": 750,
            "age": 35,
            "annualIncome": 85000,
            "monthlyIncome": 7083,
            "employmentStatus": "employed",
            "employmentYears": 5,
            "debtToIncomeRatio": 0.25,
            "_metadata": {
                "creditScore": {
                    "type": "number",
                    "description": "FICO credit score (300-850)",
                    "range": [300, 850]
                },
                "age": {
                    "type": "number",
                    "description": "Applicant age in years",
                    "range": [18, 120]
                },
                "annualIncome": {
                    "type": "number",
                    "description": "Annual income in USD",
                    "range": [0, None]
                },
                "monthlyIncome": {
                    "type": "number",
                    "description": "Monthly income in USD"
                },
                "employmentStatus": {
                    "type": "string",
                    "description": "Employment status",
                    "enum": ["employed", "unemployed", "self-employed", "retired", "student"]
                },
                "employmentYears": {
                    "type": "number",
                    "description": "Years of current employment"
                },
                "debtToIncomeRatio": {
                    "type": "number",
                    "description": "Debt-to-income ratio (0-1)"
                }
            }
        },
        "transaction": {
            "amount": 1000,
            "type": "purchase",
            "merchantCategory": "retail",
            "isInternational": False,
            "_metadata": {
                "amount": {
                    "type": "number",
                    "description": "Transaction amount in USD"
                },
                "type": {
                    "type": "string",
                    "enum": ["purchase", "cash_advance", "balance_transfer", "payment"]
                },
                "merchantCategory": {
                    "type": "string",
                    "description": "Merchant category code"
                },
                "isInternational": {
                    "type": "boolean",
                    "description": "Whether transaction is international"
                }
            }
        },
        "account": {
            "balance": 5000,
            "creditLimit": 10000,
            "availableCredit": 5000,
            "missedPayments": 0,
            "_metadata": {
                "balance": {
                    "type": "number",
                    "description": "Current account balance"
                },
                "creditLimit": {
                    "type": "number",
                    "description": "Total credit limit"
                },
                "availableCredit": {
                    "type": "number",
                    "description": "Available credit"
                },
                "missedPayments": {
                    "type": "number",
                    "description": "Number of missed payments"
                }
            }
        }
    }
}

# Test Context 1: High Credit Applicant - Approval Scenario
high_credit_context = {
    "name": "High Credit Applicant - Approval",
    "description": "Test scenario for high creditScore applicant who should be approved",
    "is_schema_template": False,
    "context_data": {
        "applicant": {
            "creditScore": 780,
            "age": 42,
            "annualIncome": 120000,
            "monthlyIncome": 10000,
            "employmentStatus": "employed",
            "employmentYears": 8,
            "debtToIncomeRatio": 0.20
        },
        "transaction": {
            "amount": 5000,
            "type": "purchase",
            "merchantCategory": "electronics",
            "isInternational": False
        },
        "account": {
            "balance": 2000,
            "creditLimit": 15000,
            "availableCredit": 13000,
            "missedPayments": 0
        }
    }
}

# Test Context 2: Low Credit Applicant - Rejection Scenario
low_credit_context = {
    "name": "Low Credit Applicant - Rejection",
    "description": "Test scenario for low creditScore applicant who should be rejected",
    "is_schema_template": False,
    "context_data": {
        "applicant": {
            "creditScore": 550,
            "age": 25,
            "annualIncome": 35000,
            "monthlyIncome": 2917,
            "employmentStatus": "employed",
            "employmentYears": 1,
            "debtToIncomeRatio": 0.45
        },
        "transaction": {
            "amount": 2000,
            "type": "cash_advance",
            "merchantCategory": "atm",
            "isInternational": False
        },
        "account": {
            "balance": 4500,
            "creditLimit": 5000,
            "availableCredit": 500,
            "missedPayments": 2
        }
    }
}

# Test Context 3: Edge Case - Missing Optional Fields
edge_case_context = {
    "name": "Edge Case - Missing Optional Data",
    "description": "Test scenario with minimal/missing data to test null handling",
    "is_schema_template": False,
    "context_data": {
        "applicant": {
            "creditScore": 650,
            "age": 30,
            "annualIncome": 60000
        },
        "transaction": {
            "amount": 1500,
            "type": "purchase"
        },
        "account": {
            "balance": 3000,
            "creditLimit": 8000
        }
    }
}

def create_contexts():
    print("🔄 Creating sample contexts...")

    contexts = [
        schema_template,
        high_credit_context,
        low_credit_context,
        edge_case_context
    ]

    for context in contexts:
        print(f"\n📝 Creating: {context['name']}")
        response = requests.post(f"{API_BASE}/contexts", json=context)

        if response.status_code == 201:
            result = response.json()
            print(f"   ✅ Created (ID: {result['id']})")
        else:
            print(f"   ❌ Failed: {response.json().get('error', 'Unknown error')}")

    print("\n✅ Sample contexts created successfully!")
    print("\n📊 Summary:")
    print("   - 1 Schema Template (defines structure)")
    print("   - 3 Test Contexts (actual test scenarios)")

if __name__ == '__main__':
    create_contexts()
