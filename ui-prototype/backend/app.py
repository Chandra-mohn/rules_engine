from flask import Flask
from flask_cors import CORS
from models import db
from api.rules import rules_bp
from api.schema import schema_bp
from api.lists import lists_bp
from api.hierarchy import hierarchy_bp
from config import Config
import os

def create_app():
    """Application factory pattern."""
    app = Flask(__name__)
    app.config.from_object(Config)
    
    # Initialize extensions
    db.init_app(app)
    CORS(app, origins=Config.CORS_ORIGINS)
    
    # Register blueprints
    app.register_blueprint(rules_bp, url_prefix='/api')
    app.register_blueprint(schema_bp, url_prefix='/api')
    app.register_blueprint(lists_bp, url_prefix='/api')
    app.register_blueprint(hierarchy_bp, url_prefix='/api')

    # ActionSets are now unified into rules API - no separate endpoint needed
    
    # Create database tables
    with app.app_context():
        # Ensure database directory exists
        os.makedirs(Config.DATABASE_PATH.parent, exist_ok=True)
        
        # Create tables
        db.create_all()
        
        # Insert sample data if database is empty
        from models import Rule, SchemaEntity, SchemaAttribute, Client, ProcessGroup, ProcessArea
        if Rule.query.count() == 0:
            # Create credit card-focused client hierarchy
            
            # Client 1: Demo Bank
            demo_client = Client(code='DEMO', name='Demo Bank', description='Demo financial institution')
            db.session.add(demo_client)
            db.session.flush()
            
            # Client 2: Premium Card Co
            premium_client = Client(code='PREMIUM', name='Premium Card Co', description='Premium credit card issuer')
            db.session.add(premium_client)
            db.session.flush()
            
            # Demo Bank Process Groups - Credit Card Focus
            cc_standard = ProcessGroup(client_id=demo_client.id, code='CC_STD', name='Standard Cards', description='Standard credit card processing')
            cc_premium = ProcessGroup(client_id=demo_client.id, code='CC_PREM', name='Premium Cards', description='Premium credit card products')
            db.session.add(cc_standard)
            db.session.add(cc_premium)
            db.session.flush()
            
            # Premium Card Co Process Groups  
            cc_platinum = ProcessGroup(client_id=premium_client.id, code='PLATINUM', name='Platinum Cards', description='Platinum tier credit cards')
            cc_rewards = ProcessGroup(client_id=premium_client.id, code='REWARDS', name='Rewards Cards', description='Cashback and rewards credit cards')
            db.session.add(cc_platinum)
            db.session.add(cc_rewards)
            db.session.flush()
            
            # Process Areas for Demo Bank - Standard Cards
            std_approval = ProcessArea(process_group_id=cc_standard.id, code='APPROVAL', name='Application Approval', description='Standard card application approval')
            std_fraud = ProcessArea(process_group_id=cc_standard.id, code='FRAUD', name='Fraud Detection', description='Standard card fraud prevention')
            db.session.add(std_approval)
            db.session.add(std_fraud)
            
            # Process Areas for Demo Bank - Premium Cards
            prem_approval = ProcessArea(process_group_id=cc_premium.id, code='PREMIUM_APPROVAL', name='Premium Approval', description='Premium card application approval')
            prem_limits = ProcessArea(process_group_id=cc_premium.id, code='CREDIT_LIMITS', name='Credit Limits', description='Premium card credit limit determination')
            db.session.add(prem_approval)
            db.session.add(prem_limits)
            
            # Process Areas for Premium Card Co - Platinum Cards
            platinum_eligibility = ProcessArea(process_group_id=cc_platinum.id, code='PLATINUM_ELIGIBILITY', name='Platinum Eligibility', description='Platinum card eligibility rules')
            
            # Process Areas for Premium Card Co - Rewards Cards
            rewards_approval = ProcessArea(process_group_id=cc_rewards.id, code='REWARDS_APPROVAL', name='Rewards Approval', description='Rewards card approval process')
            
            db.session.add(platinum_eligibility)
            db.session.add(rewards_approval)
            db.session.flush()
            
            # Now create rules distributed across credit card-focused process areas
            sample_rules = [
                # Demo Bank - Standard Card Application Approval
                Rule(
                    name='creditScoreCheck',
                    description='Basic credit score validation for standard cards',
                    content='rule creditScoreCheck:\n    if applicant.creditScore >= 700 then approveApplication\n    if applicant.creditScore < 600 then rejectApplication',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=std_approval.id
                ),
                Rule(
                    name='ageVerification',
                    description='Minimum age requirement for credit cards',
                    content='rule ageVerification:\n    if applicant.age < 18 then rejectApplication\n    if applicant.age >= 18 then approveApplication',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=std_approval.id
                ),
                Rule(
                    name='incomeVerification',
                    description='Annual income threshold for standard cards',
                    content='rule incomeVerification:\n    if applicant.annualIncome >= 50000 then approveApplication\n    if applicant.annualIncome < 25000 then rejectApplication\n    if applicant.annualIncome >= 25000 and applicant.annualIncome < 50000 then conditionalApproval',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=std_approval.id
                ),
                
                # Demo Bank - Standard Card Fraud Detection
                Rule(
                    name='businessDateCheck',
                    description='Business date validation for fraud prevention',
                    content='rule businessDateCheck:\n    if applicant.applicationDate after business_date then conditionalApproval\n    if applicant.applicationDate before business_date then rejectApplication',
                    status='DRAFT',
                    # validation_status removed - consolidated into status field
                    process_area_id=std_fraud.id
                ),
                Rule(
                    name='riskAssessment',
                    description='Combined risk factor assessment for fraud detection',
                    content='rule riskAssessment:\n    if applicant.creditScore < 600 and applicant.annualIncome < 30000 then rejectApplication\n    if applicant.age < 21 and applicant.employmentYears < 1 then rejectApplication\n    if applicant.creditScore >= 650 and applicant.employmentYears >= 3 then approveApplication',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=std_fraud.id
                ),
                Rule(
                    name='monthlyIncomeCheck',
                    description='Monthly income validation for fraud detection',
                    content='rule monthlyIncomeCheck:\n    if applicant.monthlyIncome >= 4000 then approveApplication\n    if applicant.monthlyIncome < 2000 then rejectApplication\n    if applicant.monthlyIncome >= 2000 and applicant.creditScore >= 650 then conditionalApproval',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=std_fraud.id
                ),
                
                # Demo Bank - Premium Card Approval
                Rule(
                    name='premiumEligibilityCheck',
                    description='Premium card eligibility requirements',
                    content='rule premiumEligibilityCheck:\n    if applicant.creditScore >= 750 and applicant.annualIncome >= 80000 and applicant.employmentYears >= 5 then premiumApproval\n    if applicant.creditScore >= 720 and applicant.annualIncome >= 60000 then approveApplication\n    if applicant.creditScore < 700 then rejectApplication',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=prem_approval.id
                ),
                Rule(
                    name='employmentStatusCheck',
                    description='Employment status validation for premium cards',
                    content='rule employmentStatusCheck:\n    if applicant.employmentStatus == "unemployed" then rejectApplication\n    if applicant.employmentStatus == "employed" then approveApplication\n    if applicant.employmentStatus == "self-employed" then conditionalApproval',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=prem_approval.id
                ),
                
                # Demo Bank - Premium Card Credit Limits
                Rule(
                    name='highValueApplicant',
                    description='High credit limit for qualified applicants',
                    content='rule highValueApplicant:\n    if applicant.creditScore >= 800 and applicant.annualIncome >= 100000 then fastTrackApproval\n    if applicant.creditScore >= 750 and applicant.annualIncome >= 75000 then approveApplication',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=prem_limits.id
                ),
                Rule(
                    name='employmentYearsCheck',
                    description='Employment stability for credit limit determination',
                    content='rule employmentYearsCheck:\n    if applicant.employmentYears >= 2 then approveApplication\n    if applicant.employmentYears < 1 then rejectApplication\n    if applicant.employmentYears >= 1 and applicant.employmentYears < 2 then conditionalApproval',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=prem_limits.id
                ),
                
                # Premium Card Co - Platinum Card Eligibility
                Rule(
                    name='platinumIncomeCheck',
                    description='High income requirement for platinum cards',
                    content='rule platinumIncomeCheck:\n    if applicant.annualIncome >= 150000 then approveApplication\n    if applicant.annualIncome < 100000 then rejectApplication\n    if applicant.annualIncome >= 100000 and applicant.creditScore >= 750 then conditionalApproval',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=platinum_eligibility.id
                ),
                
                # Premium Card Co - Rewards Card Approval
                Rule(
                    name='rewardsCashbackRule',
                    description='Cashback rewards card approval criteria',
                    content='rule rewardsCashbackRule:\n    if applicant.creditScore >= 680 and applicant.monthlyIncome >= 3000 then approveApplication\n    if applicant.creditScore < 650 then rejectApplication\n    if applicant.employmentStatus == "student" and applicant.age >= 21 then conditionalApproval',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=rewards_approval.id
                ),
                Rule(
                    name='rewardsPointsRule',
                    description='Points-based rewards card approval for high spenders',
                    content='rule rewardsPointsRule:\n    if applicant.creditScore >= 700 and applicant.annualIncome >= 60000 then approveApplication\n    if applicant.employmentStatus == "employed" and applicant.employmentYears >= 3 then approveApplication\n    if applicant.creditScore < 680 then rejectApplication',
                    status='VALID',
                    # validation_status removed - consolidated into status field
                    process_area_id=rewards_approval.id
                )
            ]
            
            for rule in sample_rules:
                db.session.add(rule)
            
            db.session.commit()

        # Create sample ActionSets as Rules with item_type='actionset'
        actionset_count = Rule.query.filter_by(item_type='actionset').count()
        if actionset_count == 0:
            # Create credit card-focused ActionSets as Rules with item_type='actionset'
            sample_actionsets = [
                # Demo Bank - Standard Card Application Approval ActionSets
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
                # Demo Bank - Standard Card Fraud Detection ActionSets
                Rule(
                    name='Risk Evaluation Suite',
                    description='Comprehensive risk assessment for fraud prevention',
                    content='rule riskEvaluation:\n    calculateRiskScore\n    if applicant.bankruptcyHistory == true then rejectApplication, logHighRiskApplicant\n    if applicant.employmentYears < 1 and applicant.age < 25 then requireCoSigner, scheduleManualReview\n    if applicant.monthlyIncome >= 5000 and applicant.creditScore >= 700 then approveApplication\n    else conditionalApproval',
                    status='VALID',
                    process_area_id=std_fraud.id,
                    item_type='actionset'
                ),
                # Demo Bank - Premium Card Approval ActionSets
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
                # Premium Card Co - Platinum Card Eligibility ActionSets
                Rule(
                    name='Platinum Tier Qualification',
                    description='Comprehensive qualification process for platinum cards',
                    content='rule "Platinum Tier Qualification":\n    if applicant.annualIncome >= 200000 and applicant.creditScore >= 780 then\n        platinumApproval, "assign concierge services", setupPlatinumBenefits\n    if applicant.annualIncome >= 150000 and applicant.creditScore >= 750 then\n        conditionalPlatinum, requestFinancialVerification\n    else rejectPlatinum, "suggest premium alternative"',
                    status='PROD',
                    process_area_id=platinum_eligibility.id,
                    item_type='actionset'
                ),
                # Premium Card Co - Rewards Card Approval ActionSets
                Rule(
                    name='Rewards Program Selection',
                    description='Dynamic rewards program assignment based on spending patterns',
                    content='rule rewardsProgramSelection:\n    if applicant.creditScore >= 720 then\n        if applicant.annualIncome >= 75000 then premiumRewards, "5% cashback tier"\n        else standardRewards, "2% cashback tier"\n    if applicant.employmentStatus == "student" and applicant.age >= 21 then\n        studentRewards, "1% cashback with bonus categories"\n    else basicRewards',
                    status='DRAFT',
                    process_area_id=rewards_approval.id,
                    item_type='actionset'
                )
            ]

            for actionset_rule in sample_actionsets:
                db.session.add(actionset_rule)

            db.session.commit()
            print(f"✅ Created {len(sample_actionsets)} sample ActionSets as Rules")

        # Create sample Monetary and Non-Monetary rules
        monetary_rules_count = Rule.query.filter_by(item_type='mon_rule').count()
        non_monetary_rules_count = Rule.query.filter_by(item_type='non_mon_rule').count()

        if monetary_rules_count == 0 or non_monetary_rules_count == 0:
            # Get existing process areas for the new rules
            std_approval = ProcessArea.query.filter_by(code='APPROVAL').first()
            prem_limits = ProcessArea.query.filter_by(code='CREDIT_LIMITS').first()
            std_fraud = ProcessArea.query.filter_by(code='FRAUD').first()
            prem_approval = ProcessArea.query.filter_by(code='PREMIUM_APPROVAL').first()
            platinum_eligibility = ProcessArea.query.filter_by(code='PLATINUM_ELIGIBILITY').first()
            rewards_approval = ProcessArea.query.filter_by(code='REWARDS_APPROVAL').first()
            # Sample Non-Monetary Rules (Account management, settings, cards)
            sample_non_monetary_rules = [
                Rule(
                    name='Address Update Validation',
                    description='Validate customer address change requests',
                    content='rule "Address Update Validation":\n    if applicant.addressChangeRequest == true and applicant.documentationProvided == true then\n        updateCustomerAddress(applicant.newAddress),\n        notifyCustomer("Address updated successfully")\n    else\n        requestAdditionalDocumentation',
                    status='VALID',
                    process_area_id=std_approval.id,
                    item_type='non_mon_rule'
                ),
                Rule(
                    name='Credit Limit Increase Request',
                    description='Process credit limit increase requests',
                    content='rule "Credit Limit Increase Request":\n    if account.creditScore > 750 and account.requestedLimit <= account.income * 0.3 then\n        setCreditLimit(account.requestedLimit),\n        logCreditChange("Automatic approval")\n    else\n        requireManualReview("High limit request")',
                    status='VALID',
                    process_area_id=prem_limits.id,
                    item_type='non_mon_rule'
                ),
                Rule(
                    name='Additional Card Management',
                    description='Manage additional card requests and lost card disabling',
                    content='rule "Additional Card Management":\n    if request.type == "ADD_CARD" and account.cardCount < 3 then\n        issueAdditionalCard(request.cardType),\n        updateAccountStatus("ACTIVE")\n    else if request.type == "DISABLE_CARD" and request.reason == "LOST" then\n        disableCard(request.cardNumber),\n        alertFraudDepartment(request.cardNumber)\n    else\n        rejectRequest("Card limit reached")',
                    status='DRAFT',
                    process_area_id=std_fraud.id,
                    item_type='non_mon_rule'
                ),
                Rule(
                    name='Account Settings Update',
                    description='Process account settings and preference changes',
                    content='rule "Account Settings Update":\n    if customer.requestType == "EMAIL_PREFERENCES" then\n        updateEmailPreferences(customer.preferences),\n        sendConfirmationEmail\n    else if customer.requestType == "PAYMENT_METHOD" and customer.verificationComplete == true then\n        updatePaymentMethod(customer.newPaymentMethod),\n        notifyCustomer("Payment method updated")\n    else\n        requireIdentityVerification',
                    status='VALID',
                    process_area_id=prem_approval.id,
                    item_type='non_mon_rule'
                )
            ]

            # Sample Monetary Rules (Transactions, fees, purchases)
            sample_monetary_rules = [
                Rule(
                    name='Purchase Authorization',
                    description='Authorize credit card purchase transactions',
                    content='rule "Purchase Authorization":\n    if transaction.amount <= account.creditLimit and transaction.merchant.riskLevel == "LOW" then\n        approveTransaction(transaction.amount),\n        updateAccountBalance(transaction.amount)\n    else if transaction.amount > account.creditLimit then\n        declineTransaction("Insufficient credit limit")\n    else\n        declineTransaction("High risk merchant")',
                    status='PROD',
                    process_area_id=std_fraud.id,
                    item_type='mon_rule'
                ),
                Rule(
                    name='Cash Advance Processing',
                    description='Process cash advance requests with fees',
                    content='rule "Cash Advance Processing":\n    if transaction.type == "CASH_ADVANCE" and transaction.amount <= account.cashAdvanceLimit then\n        approveCashAdvance(transaction.amount),\n        applyFee(transaction.amount * 0.03),\n        updateAccountBalance(transaction.amount + transaction.amount * 0.03)\n    else\n        declineTransaction("Cash advance limit exceeded")',
                    status='VALID',
                    process_area_id=prem_limits.id,
                    item_type='mon_rule'
                ),
                Rule(
                    name='Monthly Fee Application',
                    description='Apply monthly maintenance fees based on account tier',
                    content='rule "Monthly Fee Application":\n    if account.tier == "PREMIUM" and account.monthlySpend < 1000 then\n        applyMonthlyFee(25.00),\n        notifyCustomer("Monthly maintenance fee applied")\n    else if account.tier == "STANDARD" and account.balance < 500 then\n        applyMonthlyFee(10.00),\n        notifyCustomer("Low balance fee applied")\n    else\n        waiveFee("Spending threshold met")',
                    status='VALID',
                    process_area_id=platinum_eligibility.id,
                    item_type='mon_rule'
                ),
                Rule(
                    name='International Transaction Processing',
                    description='Process international purchases with foreign exchange fees',
                    content='rule "International Transaction Processing":\n    if transaction.location.country != "US" and transaction.amount <= account.internationalLimit then\n        approveTransaction(transaction.amount),\n        applyForeignExchangeFee(transaction.amount * 0.025),\n        updateAccountBalance(transaction.amount + transaction.amount * 0.025)\n    else if transaction.location.country != "US" and transaction.amount > account.internationalLimit then\n        declineTransaction("International transaction limit exceeded")\n    else\n        approveTransaction(transaction.amount)',
                    status='DRAFT',
                    process_area_id=rewards_approval.id,
                    item_type='mon_rule'
                )
            ]

            # Add all new rule types
            all_new_rules = sample_non_monetary_rules + sample_monetary_rules
            for rule in all_new_rules:
                db.session.add(rule)

            db.session.commit()
            print(f"✅ Created {len(sample_non_monetary_rules)} Non-Monetary rules and {len(sample_monetary_rules)} Monetary rules")

        # Insert schema data if schema tables are empty
        if SchemaEntity.query.count() == 0:
            # Create applicant entity
            applicant_entity = SchemaEntity(
                name='applicant',
                description='Credit card applicant information',
                is_active=True
            )
            db.session.add(applicant_entity)
            db.session.flush()  # Get the ID
            
            # Add applicant attributes
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
            
            # Create transaction entity
            transaction_entity = SchemaEntity(
                name='transaction',
                description='Transaction information',
                is_active=True
            )
            db.session.add(transaction_entity)
            db.session.flush()  # Get the ID
            
            # Add transaction attributes
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
            
            # Add all attributes to session
            for attr in applicant_attributes + transaction_attributes:
                db.session.add(attr)
            
            db.session.commit()
    
    @app.route('/api/health')
    def health_check():
        """Health check endpoint."""
        return {'status': 'healthy', 'message': 'Rules authoring API is running'}
    
    return app

if __name__ == '__main__':
    app = create_app()
    app.run(debug=True, host='0.0.0.0', port=5001)
