from typing import Dict, Any, List, Optional, Tuple
from sqlalchemy import or_
from models import db, Rule, RuleHistory
from services.java_bridge import JavaBridge
from config import Config

class RuleService:
    """Service class for rule management operations."""
    
    def __init__(self):
        self.java_bridge = JavaBridge(Config.JAVA_RULES_ENGINE_PATH)
    
    def get_rules(self, page: int = 1, limit: int = 10, status: Optional[str] = None, 
                  search: Optional[str] = None) -> Tuple[List[Rule], int]:
        """
        Get paginated list of rules with optional filtering.
        
        Args:
            page: Page number (1-based)
            limit: Number of rules per page
            status: Filter by status (optional)
            search: Search term for name/description (optional)
            
        Returns:
            Tuple of (rules_list, total_count)
        """
        query = Rule.query
        
        # Apply status filter
        if status:
            query = query.filter(Rule.status == status)
        
        # Apply search filter
        if search:
            search_term = f"%{search}%"
            query = query.filter(
                or_(
                    Rule.name.ilike(search_term),
                    Rule.description.ilike(search_term)
                )
            )
        
        # Get total count before pagination
        total = query.count()
        
        # Apply pagination
        rules = query.order_by(Rule.updated_at.desc()).offset((page - 1) * limit).limit(limit).all()
        
        return rules, total
    
    def get_rule_by_id(self, rule_id: int) -> Optional[Rule]:
        """Get a rule by its ID."""
        return Rule.query.get(rule_id)
    
    def get_rule_by_name(self, name: str) -> Optional[Rule]:
        """Get a rule by its name."""
        return Rule.query.filter_by(name=name).first()
    
    def create_rule(self, data: Dict[str, Any], created_by: str = 'system') -> Tuple[Rule, Dict[str, Any]]:
        """
        Create a new rule with validation.
        
        Args:
            data: Rule data (name, description, content)
            created_by: User who created the rule
            
        Returns:
            Tuple of (created_rule, validation_result)
        """
        # Validate rule content
        validation_result = self.java_bridge.validate_rule(data['content'])
        
        # Create rule
        rule = Rule(
            name=data['name'],
            description=data.get('description', ''),
            content=data['content'],
            created_by=created_by,
            updated_by=created_by,
            validation_status='valid' if validation_result['valid'] else 'invalid',
            validation_message=validation_result['message']
        )
        
        db.session.add(rule)
        db.session.commit()
        
        # Create initial history entry
        self._create_history_entry(rule, created_by, 'Initial creation')
        
        return rule, validation_result
    
    def update_rule(self, rule_id: int, data: Dict[str, Any], updated_by: str = 'system') -> Tuple[Optional[Rule], Dict[str, Any]]:
        """
        Update an existing rule with validation.
        
        Args:
            rule_id: ID of the rule to update
            data: Updated rule data
            updated_by: User who updated the rule
            
        Returns:
            Tuple of (updated_rule, validation_result)
        """
        rule = self.get_rule_by_id(rule_id)
        if not rule:
            return None, {'valid': False, 'message': 'Rule not found', 'errors': ['Rule not found']}
        
        # Store old content for history
        old_content = rule.content
        old_name = rule.name
        
        # Validate new content if provided
        validation_result = {'valid': True, 'message': 'No content changes', 'errors': []}
        if 'content' in data and data['content'] != rule.content:
            validation_result = self.java_bridge.validate_rule(data['content'])
        
        # Update rule fields
        if 'name' in data:
            rule.name = data['name']
        if 'description' in data:
            rule.description = data['description']
        if 'content' in data:
            rule.content = data['content']
        if 'status' in data:
            rule.status = data['status']
        
        rule.updated_by = updated_by
        rule.validation_status = 'valid' if validation_result['valid'] else 'invalid'
        rule.validation_message = validation_result['message']
        rule.version += 1
        
        db.session.commit()
        
        # Create history entry if content changed
        if 'content' in data and data['content'] != old_content:
            change_reason = data.get('change_reason', 'Rule content updated')
            self._create_history_entry(rule, updated_by, change_reason)
        
        return rule, validation_result
    
    def delete_rule(self, rule_id: int, deleted_by: str = 'system') -> bool:
        """
        Delete a rule (soft delete by changing status).
        
        Args:
            rule_id: ID of the rule to delete
            deleted_by: User who deleted the rule
            
        Returns:
            True if deleted successfully, False if rule not found
        """
        rule = self.get_rule_by_id(rule_id)
        if not rule:
            return False
        
        # Soft delete by changing status
        rule.status = 'deleted'
        rule.updated_by = deleted_by
        
        db.session.commit()
        
        # Create history entry
        self._create_history_entry(rule, deleted_by, 'Rule deleted')
        
        return True
    
    def validate_rule_content(self, content: str) -> Dict[str, Any]:
        """Validate rule content without saving."""
        return self.java_bridge.validate_rule(content)
    
    def test_rule(self, content: str, test_data: Dict[str, Any]) -> Dict[str, Any]:
        """Test rule execution with sample data."""
        return self.java_bridge.test_rule(content, test_data)
    
    def get_rule_history(self, rule_id: int) -> List[RuleHistory]:
        """Get history of changes for a rule."""
        return RuleHistory.query.filter_by(rule_id=rule_id).order_by(RuleHistory.created_at.desc()).all()
    
    def revert_rule(self, rule_id: int, version: int, reverted_by: str = 'system') -> Tuple[Optional[Rule], Dict[str, Any]]:
        """
        Revert a rule to a previous version.
        
        Args:
            rule_id: ID of the rule to revert
            version: Version number to revert to
            reverted_by: User who performed the revert
            
        Returns:
            Tuple of (reverted_rule, validation_result)
        """
        rule = self.get_rule_by_id(rule_id)
        if not rule:
            return None, {'valid': False, 'message': 'Rule not found', 'errors': ['Rule not found']}
        
        # Find the history entry for the target version
        history_entry = RuleHistory.query.filter_by(rule_id=rule_id, version=version).first()
        if not history_entry:
            return None, {'valid': False, 'message': 'Version not found', 'errors': ['Version not found']}
        
        # Validate the historical content
        validation_result = self.java_bridge.validate_rule(history_entry.content)
        
        # Update rule with historical content
        rule.name = history_entry.name
        rule.content = history_entry.content
        rule.updated_by = reverted_by
        rule.validation_status = 'valid' if validation_result['valid'] else 'invalid'
        rule.validation_message = validation_result['message']
        rule.version += 1
        
        db.session.commit()
        
        # Create history entry for the revert
        change_reason = f'Reverted to version {version}'
        self._create_history_entry(rule, reverted_by, change_reason)
        
        return rule, validation_result
    
    def get_autocomplete_suggestions(self, context: str, position: int) -> Dict[str, Any]:
        """Get autocomplete suggestions for rule editing."""
        return self.java_bridge.get_autocomplete_suggestions(context, position)
    
    def _create_history_entry(self, rule: Rule, created_by: str, change_reason: str):
        """Create a history entry for a rule change."""
        history = RuleHistory(
            rule_id=rule.id,
            name=rule.name,
            content=rule.content,
            version=rule.version,
            created_by=created_by,
            change_reason=change_reason
        )
        
        db.session.add(history)
        db.session.commit()