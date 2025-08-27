"""
Rules Schema Configuration
Defines the available attributes, actions, and functions for the Rules DSL.
"""

# Available attributes in the rules engine
ATTRIBUTES = {
    'applicant': {
        'description': 'Credit card applicant information',
        'properties': {
            'creditScore': {
                'type': 'number',
                'description': 'Credit score (300-850)',
                'range': [300, 850],
                'examples': ['applicant.creditScore >= 700', 'applicant.creditScore < 600']
            },
            'age': {
                'type': 'number',
                'description': 'Age in years',
                'range': [18, 120],
                'examples': ['applicant.age >= 18', 'applicant.age < 65']
            },
            'annualIncome': {
                'type': 'number',
                'description': 'Annual income in dollars',
                'range': [0, None],
                'examples': ['applicant.annualIncome > 50000', 'applicant.annualIncome >= 30000']
            },
            'monthlyIncome': {
                'type': 'number',
                'description': 'Monthly income in dollars',
                'range': [0, None],
                'examples': ['applicant.monthlyIncome > 4000']
            },
            'employmentStatus': {
                'type': 'string',
                'description': 'Employment status',
                'values': ['employed', 'unemployed', 'self-employed', 'retired', 'student'],
                'examples': ['applicant.employmentStatus = "employed"']
            },
            'employmentYears': {
                'type': 'number',
                'description': 'Years of employment',
                'range': [0, 50],
                'examples': ['applicant.employmentYears >= 2']
            },
            'applicationDate': {
                'type': 'date',
                'description': 'Application submission date',
                'examples': ['applicant.applicationDate after business_date', 'applicant.applicationDate + 30 days > today']
            },
            'birthDate': {
                'type': 'date',
                'description': 'Date of birth',
                'examples': ['applicant.birthDate before "2000-01-01"', 'year_of(now) - year_of(applicant.birthDate) >= 18']
            },
            'requestedLimit': {
                'type': 'number',
                'description': 'Requested credit limit',
                'range': [0, None],
                'examples': ['applicant.requestedLimit <= 10000']
            },
            'existingDebt': {
                'type': 'number',
                'description': 'Total existing debt',
                'range': [0, None],
                'examples': ['applicant.existingDebt < applicant.annualIncome * 0.4']
            },
            'bankruptcyHistory': {
                'type': 'boolean',
                'description': 'Has bankruptcy history',
                'examples': ['applicant.bankruptcyHistory = false']
            },
            'ssn': {
                'type': 'string',
                'description': 'Social Security Number',
                'examples': ['applicant.ssn matches "\\d{3}-\\d{2}-\\d{4}"']
            }
        }
    },
    'transaction': {
        'description': 'Transaction information',
        'properties': {
            'amount': {
                'type': 'number',
                'description': 'Transaction amount',
                'range': [0, None],
                'examples': ['transaction.amount > 1000', 'transaction.amount <= 500']
            },
            'timestamp': {
                'type': 'datetime',
                'description': 'Transaction timestamp',
                'examples': ['day_of_week(transaction.timestamp) >= 6', 'transaction.timestamp after business_date']
            },
            'merchantCategory': {
                'type': 'string',
                'description': 'Merchant category code',
                'examples': ['transaction.merchantCategory = "5411"']
            },
            'location': {
                'type': 'string',
                'description': 'Transaction location',
                'examples': ['transaction.location contains "US"']
            },
            'type': {
                'type': 'string',
                'description': 'Transaction type',
                'values': ['purchase', 'cash_advance', 'balance_transfer', 'fee'],
                'examples': ['transaction.type = "purchase"']
            },
            'isOnline': {
                'type': 'boolean',
                'description': 'Is online transaction',
                'examples': ['transaction.isOnline = true']
            }
        }
    },
    'account': {
        'description': 'Account information',
        'properties': {
            'currentBalance': {
                'type': 'number',
                'description': 'Current account balance',
                'examples': ['account.currentBalance < account.creditLimit * 0.8']
            },
            'creditLimit': {
                'type': 'number',
                'description': 'Credit limit',
                'examples': ['account.creditLimit >= 1000']
            },
            'availableCredit': {
                'type': 'number',
                'description': 'Available credit',
                'examples': ['account.availableCredit > transaction.amount']
            },
            'paymentHistory': {
                'type': 'string',
                'description': 'Payment history rating',
                'values': ['excellent', 'good', 'fair', 'poor'],
                'examples': ['account.paymentHistory = "excellent"']
            },
            'accountAge': {
                'type': 'number',
                'description': 'Account age in months',
                'examples': ['account.accountAge >= 12']
            }
        }
    }
}

# Available actions in the rules engine
ACTIONS = {
    'approveApplication': {
        'description': 'Approve the credit card application',
        'category': 'approval',
        'parameters': [],
        'examples': ['then approveApplication']
    },
    'rejectApplication': {
        'description': 'Reject the credit card application',
        'category': 'rejection',
        'parameters': [],
        'examples': ['then rejectApplication']
    },
    'conditionalApproval': {
        'description': 'Approve with conditions or lower limit',
        'category': 'approval',
        'parameters': [],
        'examples': ['then conditionalApproval']
    },
    'instantApproval': {
        'description': 'Instant approval for qualified applicants',
        'category': 'approval',
        'parameters': [],
        'examples': ['then instantApproval']
    },
    'manualReview': {
        'description': 'Require manual review by underwriter',
        'category': 'review',
        'parameters': [],
        'examples': ['then manualReview']
    },
    'requireManualReview': {
        'description': 'Flag for manual review',
        'category': 'review',
        'parameters': [],
        'examples': ['then requireManualReview']
    },
    'approveTransaction': {
        'description': 'Approve the transaction',
        'category': 'transaction',
        'parameters': [],
        'examples': ['then approveTransaction']
    },
    'declineTransaction': {
        'description': 'Decline the transaction',
        'category': 'transaction',
        'parameters': [],
        'examples': ['then declineTransaction']
    },
    'flagForReview': {
        'description': 'Flag transaction for review',
        'category': 'review',
        'parameters': [],
        'examples': ['then flagForReview']
    },
    'sendAlert': {
        'description': 'Send fraud alert',
        'category': 'alert',
        'parameters': [],
        'examples': ['then sendAlert']
    },
    'requestVerification': {
        'description': 'Request additional verification',
        'category': 'verification',
        'parameters': [],
        'examples': ['then requestVerification']
    },
    'setLimit': {
        'description': 'Set specific credit limit',
        'category': 'limit',
        'parameters': ['amount'],
        'examples': ['then setLimit(5000)']
    }
}

# Available functions in the rules engine
FUNCTIONS = {
    'date_time': {
        'now': {
            'description': 'Current timestamp',
            'return_type': 'datetime',
            'parameters': [],
            'examples': ['now', 'year_of(now)']
        },
        'today': {
            'description': 'Current date at midnight',
            'return_type': 'date',
            'parameters': [],
            'examples': ['today', 'applicant.applicationDate > today']
        },
        'business_date': {
            'description': 'Current business date (excludes weekends/holidays)',
            'return_type': 'date',
            'parameters': [],
            'examples': ['business_date', 'applicant.applicationDate >= business_date']
        },
        'year_of': {
            'description': 'Extract year from date',
            'return_type': 'number',
            'parameters': ['date'],
            'examples': ['year_of(applicant.birthDate)', 'year_of(now) - year_of(applicant.birthDate)']
        },
        'month_of': {
            'description': 'Extract month from date',
            'return_type': 'number',
            'parameters': ['date'],
            'examples': ['month_of(applicant.applicationDate)']
        },
        'day_of': {
            'description': 'Extract day from date',
            'return_type': 'number',
            'parameters': ['date'],
            'examples': ['day_of(applicant.applicationDate)']
        },
        'day_of_week': {
            'description': 'Get day of week (1=Monday, 7=Sunday)',
            'return_type': 'number',
            'parameters': ['date'],
            'examples': ['day_of_week(transaction.timestamp) >= 6']
        },
        'day_of_year': {
            'description': 'Get day of year (1-366)',
            'return_type': 'number',
            'parameters': ['date'],
            'examples': ['day_of_year(applicant.applicationDate)']
        },
        'week_of_year': {
            'description': 'Get week of year',
            'return_type': 'number',
            'parameters': ['date'],
            'examples': ['week_of_year(applicant.applicationDate)']
        }
    },
    'string': {
        'contains': {
            'description': 'Check if string contains substring',
            'return_type': 'boolean',
            'parameters': ['string', 'substring'],
            'examples': ['applicant.ssn contains "123"']
        },
        'starts_with': {
            'description': 'Check if string starts with prefix',
            'return_type': 'boolean',
            'parameters': ['string', 'prefix'],
            'examples': ['transaction.location starts_with "US"']
        },
        'ends_with': {
            'description': 'Check if string ends with suffix',
            'return_type': 'boolean',
            'parameters': ['string', 'suffix'],
            'examples': ['applicant.email ends_with "@company.com"']
        },
        'matches': {
            'description': 'Check if string matches regex pattern',
            'return_type': 'boolean',
            'parameters': ['string', 'pattern'],
            'examples': ['applicant.ssn matches "\\d{3}-\\d{2}-\\d{4}"']
        }
    }
}

# Keywords and operators
KEYWORDS = [
    'rule', 'if', 'then', 'else', 'and', 'or', 'not',
    'true', 'false', 'null',
    'before', 'after', 'between', 'within',
    'in', 'is_weekend', 'is_weekday', 'is_holiday'
]

OPERATORS = [
    '=', '!=', '<', '<=', '>', '>=',
    '+', '-', '*', '/', '%'
]

# Time units
TIME_UNITS = [
    'years', 'year', 'months', 'month', 'weeks', 'week',
    'days', 'day', 'hours', 'hour', 'minutes', 'minute',
    'seconds', 'second'
]

def get_all_attributes():
    """Get all attributes as a flat list for autocomplete."""
    attributes = []
    for entity_name, entity_data in ATTRIBUTES.items():
        for prop_name, prop_data in entity_data['properties'].items():
            attributes.append({
                'label': f'{entity_name}.{prop_name}',
                'kind': 'property',
                'detail': prop_data['type'],
                'documentation': prop_data['description'],
                'examples': prop_data.get('examples', [])
            })
    return attributes

def get_all_actions():
    """Get all actions as a list for autocomplete."""
    actions = []
    for action_name, action_data in ACTIONS.items():
        actions.append({
            'label': action_name,
            'kind': 'function',
            'detail': 'action',
            'documentation': action_data['description'],
            'category': action_data['category'],
            'examples': action_data.get('examples', [])
        })
    return actions

def get_all_functions():
    """Get all functions as a list for autocomplete."""
    functions = []
    for category_name, category_functions in FUNCTIONS.items():
        for func_name, func_data in category_functions.items():
            functions.append({
                'label': func_name,
                'kind': 'function',
                'detail': func_data['return_type'],
                'documentation': func_data['description'],
                'parameters': func_data['parameters'],
                'examples': func_data.get('examples', [])
            })
    return functions

def get_attributes_by_entity(entity_name):
    """Get attributes for a specific entity (e.g., 'applicant')."""
    if entity_name not in ATTRIBUTES:
        return []
    
    attributes = []
    entity_data = ATTRIBUTES[entity_name]
    for prop_name, prop_data in entity_data['properties'].items():
        attributes.append({
            'label': f'{entity_name}.{prop_name}',
            'kind': 'property',
            'detail': prop_data['type'],
            'documentation': prop_data['description'],
            'examples': prop_data.get('examples', [])
        })
    return attributes