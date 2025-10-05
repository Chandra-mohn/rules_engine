"""
Demo Data Fixtures for Rules Engine

This module contains all demo data creation logic, extracted from app.py.
Provides functions to seed the database with sample data for development and testing.
"""

from models import db, Rule, SchemaEntity, SchemaAttribute, Client, ProcessGroup, ProcessArea


def clear_all_data():
    """Clear all demo data from the database. USE WITH CAUTION!"""
    print("🗑️  Clearing all data from database...")

    # Delete in order of dependencies
    Rule.query.delete()
    SchemaAttribute.query.delete()
    SchemaEntity.query.delete()
    ProcessArea.query.delete()
    ProcessGroup.query.delete()
    Client.query.delete()

    db.session.commit()
    print("✅ All data cleared")


def create_demo_data():
    """
    Create comprehensive demo data for the rules engine.

    This creates:
    - 2 Clients (Demo Bank, Premium Card Co)
    - 4 Process Groups (Standard Cards, Premium Cards, Platinum Cards, Rewards Cards)
    - 6 Process Areas (various approval and fraud detection areas)
    - 12 Sample Rules (various validation rules)
    - 7 ActionSets (workflow compositions)
    - 8 Monetary/Non-Monetary Rules
    - 2 Schema Entities with attributes

    Returns:
        dict: Summary of created data
    """
    print("\n" + "=" * 80)
    print("📦 Creating Demo Data for Rules Engine")
    print("=" * 80)

    # Track what we create
    created = {
        'clients': 0,
        'process_groups': 0,
        'process_areas': 0,
        'rules': 0,
        'actionsets': 0,
        'actions': 0,
        'mon_rules': 0,
        'non_mon_rules': 0,
        'schema_entities': 0,
        'schema_attributes': 0
    }

    # === CLIENTS ===
    print("\n1️⃣  Creating Clients...")
    demo_client = Client(code='DEMO', name='Demo Bank', description='Demo financial institution')
    premium_client = Client(code='PREMIUM', name='Premium Card Co', description='Premium credit card issuer')
    db.session.add(demo_client)
    db.session.add(premium_client)
    db.session.flush()
    created['clients'] = 2
    print(f"   ✅ Created {created['clients']} clients")

    # === PROCESS GROUPS ===
    print("\n2️⃣  Creating Process Groups...")
    cc_standard = ProcessGroup(client_id=demo_client.id, code='CC_STD', name='Standard Cards', description='Standard credit card processing')
    cc_premium = ProcessGroup(client_id=demo_client.id, code='CC_PREM', name='Premium Cards', description='Premium credit card products')
    cc_platinum = ProcessGroup(client_id=premium_client.id, code='PLATINUM', name='Platinum Cards', description='Platinum tier credit cards')
    cc_rewards = ProcessGroup(client_id=premium_client.id, code='REWARDS', name='Rewards Cards', description='Cashback and rewards credit cards')

    for pg in [cc_standard, cc_premium, cc_platinum, cc_rewards]:
        db.session.add(pg)
    db.session.flush()
    created['process_groups'] = 4
    print(f"   ✅ Created {created['process_groups']} process groups")

    # === PROCESS AREAS ===
    print("\n3️⃣  Creating Process Areas...")
    process_areas_list = [
        ProcessArea(process_group_id=cc_standard.id, code='APPROVAL', name='Application Approval', description='Standard card application approval'),
        ProcessArea(process_group_id=cc_standard.id, code='FRAUD', name='Fraud Detection', description='Standard card fraud prevention'),
        ProcessArea(process_group_id=cc_premium.id, code='PREMIUM_APPROVAL', name='Premium Approval', description='Premium card application approval'),
        ProcessArea(process_group_id=cc_premium.id, code='CREDIT_LIMITS', name='Credit Limits', description='Premium card credit limit determination'),
        ProcessArea(process_group_id=cc_platinum.id, code='PLATINUM_ELIGIBILITY', name='Platinum Eligibility', description='Platinum card eligibility rules'),
        ProcessArea(process_group_id=cc_rewards.id, code='REWARDS_APPROVAL', name='Rewards Approval', description='Rewards card approval process'),
    ]

    for pa in process_areas_list:
        db.session.add(pa)
    db.session.flush()
    created['process_areas'] = len(process_areas_list)
    print(f"   ✅ Created {created['process_areas']} process areas")

    # Store references by code for easier access
    process_areas = {pa.code: pa for pa in process_areas_list}
    std_approval = process_areas['APPROVAL']
    std_fraud = process_areas['FRAUD']
    prem_approval = process_areas['PREMIUM_APPROVAL']
    prem_limits = process_areas['CREDIT_LIMITS']
    platinum_eligibility = process_areas['PLATINUM_ELIGIBILITY']
    rewards_approval = process_areas['REWARDS_APPROVAL']

    # === SAMPLE RULES ===
    print("\n4️⃣  Creating Sample Rules...")
    sample_rules = [
        # Demo Bank - Standard Card Application Approval
        Rule(
            name='creditScoreCheck',
            description='Basic credit score validation for standard cards',
            content='rule creditScoreCheck:\n    if applicant.creditScore >= 700 then approveApplication\n    if applicant.creditScore < 600 then rejectApplication',
            status='VALID',
            process_area_id=std_approval.id
        ),
        Rule(
            name='ageVerification',
            description='Minimum age requirement for credit cards',
            content='rule ageVerification:\n    if applicant.age < 18 then rejectApplication\n    if applicant.age >= 18 then approveApplication',
            status='VALID',
            process_area_id=std_approval.id
        ),
        Rule(
            name='incomeVerification',
            description='Annual income threshold for standard cards',
            content='rule incomeVerification:\n    if applicant.annualIncome >= 50000 then approveApplication\n    if applicant.annualIncome < 25000 then rejectApplication\n    if applicant.annualIncome >= 25000 and applicant.annualIncome < 50000 then conditionalApproval',
            status='VALID',
            process_area_id=std_approval.id
        ),

        # Demo Bank - Standard Card Fraud Detection
        Rule(
            name='businessDateCheck',
            description='Business date validation for fraud prevention',
            content='rule businessDateCheck:\n    if applicant.applicationDate after business_date then conditionalApproval\n    if applicant.applicationDate before business_date then rejectApplication',
            status='DRAFT',
            process_area_id=std_fraud.id
        ),
        Rule(
            name='riskAssessment',
            description='Combined risk factor assessment for fraud detection',
            content='rule riskAssessment:\n    if applicant.creditScore < 600 and applicant.annualIncome < 30000 then rejectApplication\n    if applicant.age < 21 and applicant.employmentYears < 1 then rejectApplication\n    if applicant.creditScore >= 650 and applicant.employmentYears >= 3 then approveApplication',
            status='VALID',
            process_area_id=std_fraud.id
        ),
        Rule(
            name='monthlyIncomeCheck',
            description='Monthly income validation for fraud detection',
            content='rule monthlyIncomeCheck:\n    if applicant.monthlyIncome >= 4000 then approveApplication\n    if applicant.monthlyIncome < 2000 then rejectApplication\n    if applicant.monthlyIncome >= 2000 and applicant.creditScore >= 650 then conditionalApproval',
            status='VALID',
            process_area_id=std_fraud.id
        ),

        # Demo Bank - Premium Card Approval
        Rule(
            name='premiumEligibilityCheck',
            description='Premium card eligibility requirements',
            content='rule premiumEligibilityCheck:\n    if applicant.creditScore >= 750 and applicant.annualIncome >= 80000 and applicant.employmentYears >= 5 then premiumApproval\n    if applicant.creditScore >= 720 and applicant.annualIncome >= 60000 then approveApplication\n    if applicant.creditScore < 700 then rejectApplication',
            status='VALID',
            process_area_id=prem_approval.id
        ),
        Rule(
            name='employmentStatusCheck',
            description='Employment status validation for premium cards',
            content='rule employmentStatusCheck:\n    if applicant.employmentStatus == "unemployed" then rejectApplication\n    if applicant.employmentStatus == "employed" then approveApplication\n    if applicant.employmentStatus == "self-employed" then conditionalApproval',
            status='VALID',
            process_area_id=prem_approval.id
        ),

        # Demo Bank - Premium Card Credit Limits
        Rule(
            name='highValueApplicant',
            description='High credit limit for qualified applicants',
            content='rule highValueApplicant:\n    if applicant.creditScore >= 800 and applicant.annualIncome >= 100000 then fastTrackApproval\n    if applicant.creditScore >= 750 and applicant.annualIncome >= 75000 then approveApplication',
            status='VALID',
            process_area_id=prem_limits.id
        ),
        Rule(
            name='employmentYearsCheck',
            description='Employment stability for credit limit determination',
            content='rule employmentYearsCheck:\n    if applicant.employmentYears >= 2 then approveApplication\n    if applicant.employmentYears < 1 then rejectApplication\n    if applicant.employmentYears >= 1 and applicant.employmentYears < 2 then conditionalApproval',
            status='VALID',
            process_area_id=prem_limits.id
        ),

        # Premium Card Co - Platinum Card Eligibility
        Rule(
            name='platinumIncomeCheck',
            description='High income requirement for platinum cards',
            content='rule platinumIncomeCheck:\n    if applicant.annualIncome >= 150000 then approveApplication\n    if applicant.annualIncome < 100000 then rejectApplication\n    if applicant.annualIncome >= 100000 and applicant.creditScore >= 750 then conditionalApproval',
            status='VALID',
            process_area_id=platinum_eligibility.id
        ),

        # Premium Card Co - Rewards Card Approval
        Rule(
            name='rewardsCashbackRule',
            description='Cashback rewards card approval criteria',
            content='rule rewardsCashbackRule:\n    if applicant.creditScore >= 680 and applicant.monthlyIncome >= 3000 then approveApplication\n    if applicant.creditScore < 650 then rejectApplication\n    if applicant.employmentStatus == "student" and applicant.age >= 21 then conditionalApproval',
            status='VALID',
            process_area_id=rewards_approval.id
        ),
        Rule(
            name='rewardsPointsRule',
            description='Points-based rewards card approval for high spenders',
            content='rule rewardsPointsRule:\n    if applicant.creditScore >= 700 and applicant.annualIncome >= 60000 then approveApplication\n    if applicant.employmentStatus == "employed" and applicant.employmentYears >= 3 then approveApplication\n    if applicant.creditScore < 680 then rejectApplication',
            status='VALID',
            process_area_id=rewards_approval.id
        )
    ]

    for rule in sample_rules:
        db.session.add(rule)
    created['rules'] = len(sample_rules)
    print(f"   ✅ Created {created['rules']} sample rules")

    # === ACTIONSETS ===
    print("\n5️⃣  Creating ActionSets...")
    sample_actionsets = [
        Rule(
            name='Standard Application Workflow',
            description='Complete workflow for standard credit card applications',
            content='rule "Standard Application Workflow":\n    validateApplicantInfo\n    if applicant.creditScore >= 700 then approveApplication, sendWelcomeEmail\n    if applicant.creditScore < 600 then rejectApplication, sendRejectionLetter\n    else conditionalApproval, requestDocumentation\n    updateCustomerRecord',
            status='VALID',
            process_area_id=std_approval.id,
            item_type='actionset'
        ),
        Rule(
            name='Quick Credit Assessment',
            description='Fast credit scoring for instant decisions',
            content='rule quickCreditAssessment:\n    calculateRiskScore\n    if applicant.creditScore >= 750 and applicant.annualIncome >= 60000 then instantApproval\n    if applicant.creditScore < 550 then immediateReject\n    else "Standard Application Workflow"',
            status='DRAFT',
            process_area_id=std_approval.id,
            item_type='actionset'
        ),
        Rule(
            name='Risk Evaluation Suite',
            description='Comprehensive risk assessment for fraud prevention',
            content='rule riskEvaluation:\n    calculateRiskScore\n    if applicant.bankruptcyHistory == true then rejectApplication, logHighRiskApplicant\n    if applicant.employmentYears < 1 and applicant.age < 25 then requireCoSigner, scheduleManualReview\n    if applicant.monthlyIncome >= 5000 and applicant.creditScore >= 700 then approveApplication\n    else conditionalApproval',
            status='VALID',
            process_area_id=std_fraud.id,
            item_type='actionset'
        ),
        Rule(
            name='Premium Card Processing',
            description='Specialized workflow for premium credit card applications',
            content='rule "Premium Card Processing":\n    if applicant.annualIncome >= 100000 and applicant.creditScore >= 750 then\n        instantApproval, assignPremiumBenefits, "send platinum welcome package"\n    if applicant.creditScore >= 700 and applicant.employmentStatus == "employed" then\n        approveApplication, "assign standard benefits"\n    else "Standard Application Workflow"',
            status='PROD',
            process_area_id=prem_approval.id,
            item_type='actionset'
        ),
        Rule(
            name='High Net Worth Processing',
            description='Expedited processing for high-value clients',
            content='rule highNetWorthProcessing:\n    if applicant.annualIncome >= 250000 then fastTrackApproval, assignPrivateBanker\n    if applicant.creditScore >= 800 and applicant.employmentYears >= 5 then premiumApproval\n    else conditionalApproval, schedulePersonalConsultation',
            status='VALID',
            process_area_id=prem_limits.id,
            item_type='actionset'
        ),
        Rule(
            name='Platinum Tier Qualification',
            description='Comprehensive qualification process for platinum cards',
            content='rule "Platinum Tier Qualification":\n    if applicant.annualIncome >= 200000 and applicant.creditScore >= 780 then\n        platinumApproval, "assign concierge services", setupPlatinumBenefits\n    if applicant.annualIncome >= 150000 and applicant.creditScore >= 750 then\n        conditionalPlatinum, requestFinancialVerification\n    else rejectPlatinum, "suggest premium alternative"',
            status='PROD',
            process_area_id=platinum_eligibility.id,
            item_type='actionset'
        ),
        Rule(
            name='Rewards Program Selection',
            description='Dynamic rewards program assignment based on spending patterns',
            content='rule rewardsProgramSelection:\n    if applicant.creditScore >= 720 then\n        if applicant.annualIncome >= 75000 then premiumRewards, "5% cashback tier"\n        else standardRewards, "2% cashback tier"\n    if applicant.employmentStatus == "student" and applicant.age >= 21 then\n        studentRewards, "1% cashback with bonus categories"\n    else basicRewards',
            status='DRAFT',
            process_area_id=rewards_approval.id,
            item_type='actionset'
        )
    ]

    for actionset in sample_actionsets:
        db.session.add(actionset)
    created['actionsets'] = len(sample_actionsets)
    print(f"   ✅ Created {created['actionsets']} actionsets")

    # === MONETARY & NON-MONETARY RULES ===
    print("\n6️⃣  Creating Monetary & Non-Monetary Rules...")

    # Get process areas safely (they should exist from above, but be defensive)
    pa_lookup = {pa.code: pa for pa in ProcessArea.query.all()}

    # Use whatever process areas are available
    available_areas = list(pa_lookup.values())
    if len(available_areas) < 6:
        print(f"   ⚠️  Warning: Only {len(available_areas)} process areas available. Using first available for all rules.")
        # Use first available for all rules
        default_pa = available_areas[0]
        sample_non_monetary_rules = [
            Rule(
                name='Address Update Validation',
                description='Validate customer address change requests',
                content='rule "Address Update Validation":\n    if applicant.addressChangeRequest == true and applicant.documentationProvided == true then\n        updateCustomerAddress(applicant.newAddress),\n        notifyCustomer("Address updated successfully")\n    else\n        requestAdditionalDocumentation',
                status='VALID',
                process_area_id=default_pa.id,
                item_type='non_mon_rule'
            ),
            Rule(
                name='Credit Limit Increase Request',
                description='Process credit limit increase requests',
                content='rule "Credit Limit Increase Request":\n    if account.creditScore > 750 and account.requestedLimit <= account.income * 0.3 then\n        setCreditLimit(account.requestedLimit),\n        logCreditChange("Automatic approval")\n    else\n        requireManualReview("High limit request")',
                status='VALID',
                process_area_id=default_pa.id,
                item_type='non_mon_rule'
            ),
            Rule(
                name='Additional Card Management',
                description='Manage additional card requests and lost card disabling',
                content='rule "Additional Card Management":\n    if request.type == "ADD_CARD" and account.cardCount < 3 then\n        issueAdditionalCard(request.cardType),\n        updateAccountStatus("ACTIVE")\n    else if request.type == "DISABLE_CARD" and request.reason == "LOST" then\n        disableCard(request.cardNumber),\n        alertFraudDepartment(request.cardNumber)\n    else\n        rejectRequest("Card limit reached")',
                status='DRAFT',
                process_area_id=default_pa.id,
                item_type='non_mon_rule'
            ),
            Rule(
                name='Account Settings Update',
                description='Process account settings and preference changes',
                content='rule "Account Settings Update":\n    if customer.requestType == "EMAIL_PREFERENCES" then\n        updateEmailPreferences(customer.preferences),\n        sendConfirmationEmail\n    else if customer.requestType == "PAYMENT_METHOD" and customer.verificationComplete == true then\n        updatePaymentMethod(customer.newPaymentMethod),\n        notifyCustomer("Payment method updated")\n    else\n        requireIdentityVerification',
                status='VALID',
                process_area_id=default_pa.id,
                item_type='non_mon_rule'
            )
        ]

        sample_monetary_rules = [
            Rule(
                name='Purchase Authorization',
                description='Authorize credit card purchase transactions',
                content='rule "Purchase Authorization":\n    if transaction.amount <= account.creditLimit and transaction.merchant.riskLevel == "LOW" then\n        approveTransaction(transaction.amount),\n        updateAccountBalance(transaction.amount)\n    else if transaction.amount > account.creditLimit then\n        declineTransaction("Insufficient credit limit")\n    else\n        declineTransaction("High risk merchant")',
                status='PROD',
                process_area_id=default_pa.id,
                item_type='mon_rule'
            ),
            Rule(
                name='Cash Advance Processing',
                description='Process cash advance requests with fees',
                content='rule "Cash Advance Processing":\n    if transaction.type == "CASH_ADVANCE" and transaction.amount <= account.cashAdvanceLimit then\n        approveCashAdvance(transaction.amount),\n        applyFee(transaction.amount * 0.03),\n        updateAccountBalance(transaction.amount + transaction.amount * 0.03)\n    else\n        declineTransaction("Cash advance limit exceeded")',
                status='VALID',
                process_area_id=default_pa.id,
                item_type='mon_rule'
            ),
            Rule(
                name='Monthly Fee Application',
                description='Apply monthly maintenance fees based on account tier',
                content='rule "Monthly Fee Application":\n    if account.tier == "PREMIUM" and account.monthlySpend < 1000 then\n        applyMonthlyFee(25.00),\n        notifyCustomer("Monthly maintenance fee applied")\n    else if account.tier == "STANDARD" and account.balance < 500 then\n        applyMonthlyFee(10.00),\n        notifyCustomer("Low balance fee applied")\n    else\n        waiveFee("Spending threshold met")',
                status='VALID',
                process_area_id=default_pa.id,
                item_type='mon_rule'
            ),
            Rule(
                name='International Transaction Processing',
                description='Process international purchases with foreign exchange fees',
                content='rule "International Transaction Processing":\n    if transaction.location.country != "US" and transaction.amount <= account.internationalLimit then\n        approveTransaction(transaction.amount),\n        applyForeignExchangeFee(transaction.amount * 0.025),\n        updateAccountBalance(transaction.amount + transaction.amount * 0.025)\n    else if transaction.location.country != "US" and transaction.amount > account.internationalLimit then\n        declineTransaction("International transaction limit exceeded")\n    else\n        approveTransaction(transaction.amount)',
                status='DRAFT',
                process_area_id=default_pa.id,
                item_type='mon_rule'
            )
        ]
    else:
        # Use specific process areas by code
        sample_non_monetary_rules = [
            Rule(
                name='Address Update Validation',
                description='Validate customer address change requests',
                content='rule "Address Update Validation":\n    if applicant.addressChangeRequest == true and applicant.documentationProvided == true then\n        updateCustomerAddress(applicant.newAddress),\n        notifyCustomer("Address updated successfully")\n    else\n        requestAdditionalDocumentation',
                status='VALID',
                process_area_id=pa_lookup.get('APPROVAL', available_areas[0]).id,
                item_type='non_mon_rule'
            ),
            Rule(
                name='Credit Limit Increase Request',
                description='Process credit limit increase requests',
                content='rule "Credit Limit Increase Request":\n    if account.creditScore > 750 and account.requestedLimit <= account.income * 0.3 then\n        setCreditLimit(account.requestedLimit),\n        logCreditChange("Automatic approval")\n    else\n        requireManualReview("High limit request")',
                status='VALID',
                process_area_id=pa_lookup.get('CREDIT_LIMITS', available_areas[0]).id,
                item_type='non_mon_rule'
            ),
            Rule(
                name='Additional Card Management',
                description='Manage additional card requests and lost card disabling',
                content='rule "Additional Card Management":\n    if request.type == "ADD_CARD" and account.cardCount < 3 then\n        issueAdditionalCard(request.cardType),\n        updateAccountStatus("ACTIVE")\n    else if request.type == "DISABLE_CARD" and request.reason == "LOST" then\n        disableCard(request.cardNumber),\n        alertFraudDepartment(request.cardNumber)\n    else\n        rejectRequest("Card limit reached")',
                status='DRAFT',
                process_area_id=pa_lookup.get('FRAUD', available_areas[0]).id,
                item_type='non_mon_rule'
            ),
            Rule(
                name='Account Settings Update',
                description='Process account settings and preference changes',
                content='rule "Account Settings Update":\n    if customer.requestType == "EMAIL_PREFERENCES" then\n        updateEmailPreferences(customer.preferences),\n        sendConfirmationEmail\n    else if customer.requestType == "PAYMENT_METHOD" and customer.verificationComplete == true then\n        updatePaymentMethod(customer.newPaymentMethod),\n        notifyCustomer("Payment method updated")\n    else\n        requireIdentityVerification',
                status='VALID',
                process_area_id=pa_lookup.get('PREMIUM_APPROVAL', available_areas[0]).id,
                item_type='non_mon_rule'
            )
        ]

        sample_monetary_rules = [
            Rule(
                name='Purchase Authorization',
                description='Authorize credit card purchase transactions',
                content='rule "Purchase Authorization":\n    if transaction.amount <= account.creditLimit and transaction.merchant.riskLevel == "LOW" then\n        approveTransaction(transaction.amount),\n        updateAccountBalance(transaction.amount)\n    else if transaction.amount > account.creditLimit then\n        declineTransaction("Insufficient credit limit")\n    else\n        declineTransaction("High risk merchant")',
                status='PROD',
                process_area_id=pa_lookup.get('FRAUD', available_areas[0]).id,
                item_type='mon_rule'
            ),
            Rule(
                name='Cash Advance Processing',
                description='Process cash advance requests with fees',
                content='rule "Cash Advance Processing":\n    if transaction.type == "CASH_ADVANCE" and transaction.amount <= account.cashAdvanceLimit then\n        approveCashAdvance(transaction.amount),\n        applyFee(transaction.amount * 0.03),\n        updateAccountBalance(transaction.amount + transaction.amount * 0.03)\n    else\n        declineTransaction("Cash advance limit exceeded")',
                status='VALID',
                process_area_id=pa_lookup.get('CREDIT_LIMITS', available_areas[0]).id,
                item_type='mon_rule'
            ),
            Rule(
                name='Monthly Fee Application',
                description='Apply monthly maintenance fees based on account tier',
                content='rule "Monthly Fee Application":\n    if account.tier == "PREMIUM" and account.monthlySpend < 1000 then\n        applyMonthlyFee(25.00),\n        notifyCustomer("Monthly maintenance fee applied")\n    else if account.tier == "STANDARD" and account.balance < 500 then\n        applyMonthlyFee(10.00),\n        notifyCustomer("Low balance fee applied")\n    else\n        waiveFee("Spending threshold met")',
                status='VALID',
                process_area_id=pa_lookup.get('PLATINUM_ELIGIBILITY', available_areas[0]).id,
                item_type='mon_rule'
            ),
            Rule(
                name='International Transaction Processing',
                description='Process international purchases with foreign exchange fees',
                content='rule "International Transaction Processing":\n    if transaction.location.country != "US" and transaction.amount <= account.internationalLimit then\n        approveTransaction(transaction.amount),\n        applyForeignExchangeFee(transaction.amount * 0.025),\n        updateAccountBalance(transaction.amount + transaction.amount * 0.025)\n    else if transaction.location.country != "US" and transaction.amount > account.internationalLimit then\n        declineTransaction("International transaction limit exceeded")\n    else\n        approveTransaction(transaction.amount)',
                status='DRAFT',
                process_area_id=pa_lookup.get('REWARDS_APPROVAL', available_areas[0]).id,
                item_type='mon_rule'
            )
        ]

    for rule in sample_non_monetary_rules + sample_monetary_rules:
        db.session.add(rule)
    created['non_mon_rules'] = len(sample_non_monetary_rules)
    created['mon_rules'] = len(sample_monetary_rules)
    print(f"   ✅ Created {created['non_mon_rules']} non-monetary rules")
    print(f"   ✅ Created {created['mon_rules']} monetary rules")

    # === SCHEMA ENTITIES & ATTRIBUTES ===
    print("\n7️⃣  Creating Schema Entities & Attributes...")

    # Applicant entity
    applicant_entity = SchemaEntity(
        name='applicant',
        description='Credit card applicant information',
        is_active=True
    )
    db.session.add(applicant_entity)
    db.session.flush()

    applicant_attributes = [
        SchemaAttribute(entity_id=applicant_entity.id, name='creditScore', data_type='number', java_type='int',
                      min_value=300, max_value=850, description='Credit score (300-850)'),
        SchemaAttribute(entity_id=applicant_entity.id, name='age', data_type='number', java_type='int',
                      min_value=18, max_value=120, description='Age in years'),
        SchemaAttribute(entity_id=applicant_entity.id, name='annualIncome', data_type='number', java_type='int',
                      min_value=0, description='Annual income in dollars'),
        SchemaAttribute(entity_id=applicant_entity.id, name='monthlyIncome', data_type='number', java_type='int',
                      min_value=0, description='Monthly income in dollars'),
        SchemaAttribute(entity_id=applicant_entity.id, name='employmentStatus', data_type='string', java_type='String',
                      allowed_values='["employed", "unemployed", "self-employed", "retired", "student"]',
                      description='Employment status'),
        SchemaAttribute(entity_id=applicant_entity.id, name='employmentYears', data_type='number', java_type='int',
                      min_value=0, max_value=50, description='Years of employment'),
        SchemaAttribute(entity_id=applicant_entity.id, name='applicationDate', data_type='date', java_type='LocalDate',
                      description='Application submission date')
    ]

    # Transaction entity
    transaction_entity = SchemaEntity(
        name='transaction',
        description='Transaction information',
        is_active=True
    )
    db.session.add(transaction_entity)
    db.session.flush()

    transaction_attributes = [
        SchemaAttribute(entity_id=transaction_entity.id, name='amount', data_type='number', java_type='BigDecimal',
                      min_value=0, description='Transaction amount'),
        SchemaAttribute(entity_id=transaction_entity.id, name='currency', data_type='string', java_type='String',
                      allowed_values='["USD", "EUR", "GBP", "CAD"]', description='Transaction currency'),
        SchemaAttribute(entity_id=transaction_entity.id, name='type', data_type='string', java_type='String',
                      allowed_values='["purchase", "cash_advance", "balance_transfer"]', description='Transaction type'),
        SchemaAttribute(entity_id=transaction_entity.id, name='merchantCategory', data_type='string', java_type='String',
                      description='Merchant category code')
    ]

    for attr in applicant_attributes + transaction_attributes:
        db.session.add(attr)

    created['schema_entities'] = 2
    created['schema_attributes'] = len(applicant_attributes) + len(transaction_attributes)
    print(f"   ✅ Created {created['schema_entities']} schema entities")
    print(f"   ✅ Created {created['schema_attributes']} schema attributes")

    # === ACTIONS ===
    print("\n8️⃣  Creating Sample Actions...")

    # Get first process area for global actions (workaround for NOT NULL constraint)
    # TODO: Consider making process_area_id nullable for global actions
    first_process_area = ProcessArea.query.first()

    sample_actions = [
        Rule(
            name='approve',
            description='Approve a credit card application or transaction',
            content='com/rules/actions/ApprovalAction.java',  # Just the file path
            status='VALID',
            process_area_id=first_process_area.id,
            item_type='action'
        ),
        Rule(
            name='reject',
            description='Reject a credit card application or transaction',
            content='com/rules/actions/RejectionAction.java',  # Just the file path
            status='VALID',
            process_area_id=first_process_area.id,
            item_type='action'
        ),
        Rule(
            name='sendEmail',
            description='Send email notification to applicant or customer',
            content='com/rules/actions/EmailAction.java',  # Just the file path
            status='VALID',
            process_area_id=first_process_area.id,
            item_type='action'
        )
    ]

    for action in sample_actions:
        db.session.add(action)

    created['actions'] = len(sample_actions)
    print(f"   ✅ Created {created['actions']} actions")

    # === COMMIT ALL ===
    db.session.commit()

    print("\n" + "=" * 80)
    print("✅ Demo Data Creation Complete!")
    print("=" * 80)
    print(f"\n📊 Summary:")
    for key, value in created.items():
        print(f"   {key.replace('_', ' ').title()}: {value}")
    print()

    return created
