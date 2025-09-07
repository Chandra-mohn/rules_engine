"""
List Cache Service for Named Lists in Rules Engine
Provides lazy loading and caching for rule list definitions.
"""

import time
import json
import threading
from typing import Optional, Set, Dict, Any, List
from models import db, RuleList


class ListCache:
    """Thread-safe cache for named rule lists with lazy loading."""
    
    _instance = None
    _lock = threading.Lock()
    
    def __new__(cls):
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = super().__new__(cls)
                    cls._instance._initialize()
        return cls._instance
    
    def _initialize(self):
        """Initialize the cache."""
        self._cache: Dict[str, Set] = {}
        self._metadata: Dict[str, Dict[str, Any]] = {}
        self._last_refresh = 0
        self._refresh_interval = 300  # 5 minutes
        self._cache_lock = threading.RLock()
    
    def get_list(self, name: str, schema_version: str = 'both') -> Optional[Set]:
        """
        Get a named list as a set for fast membership testing.
        
        Args:
            name: Name of the list
            schema_version: Schema version filter ('modern', 'legacy', 'both')
            
        Returns:
            Set of values or None if list doesn't exist
        """
        with self._cache_lock:
            self._ensure_fresh()
            
            # Check if list exists and matches schema version
            if name in self._metadata:
                list_schema = self._metadata[name]['schema_version']
                if self._is_schema_compatible(list_schema, schema_version):
                    return self._cache.get(name)
            
            return None
    
    def get_list_metadata(self, name: str) -> Optional[Dict[str, Any]]:
        """Get metadata for a named list."""
        with self._cache_lock:
            self._ensure_fresh()
            return self._metadata.get(name)
    
    def get_all_lists(self, schema_version: str = 'both') -> Dict[str, Dict[str, Any]]:
        """Get all lists metadata filtered by schema version."""
        with self._cache_lock:
            self._ensure_fresh()
            
            result = {}
            for name, metadata in self._metadata.items():
                list_schema = metadata['schema_version']
                if self._is_schema_compatible(list_schema, schema_version):
                    result[name] = metadata.copy()
                    result[name]['values'] = list(self._cache[name])  # Convert set back to list
            
            return result
    
    def invalidate(self, name: Optional[str] = None):
        """
        Invalidate cache for specific list or all lists.
        
        Args:
            name: List name to invalidate, or None for all lists
        """
        with self._cache_lock:
            if name:
                self._cache.pop(name, None)
                self._metadata.pop(name, None)
            else:
                self._cache.clear()
                self._metadata.clear()
            self._last_refresh = 0  # Force refresh on next access
    
    def refresh(self):
        """Force refresh of the cache from database."""
        with self._cache_lock:
            self._load_from_database()
    
    def _ensure_fresh(self):
        """Ensure cache is fresh, refresh if needed."""
        current_time = time.time()
        if current_time - self._last_refresh > self._refresh_interval:
            self._load_from_database()
    
    def _load_from_database(self):
        """Load all lists from database into cache."""
        try:
            from flask import current_app
            
            # Need application context for database access
            with current_app.app_context():
                rule_lists = RuleList.query.all()
                
                new_cache = {}
                new_metadata = {}
                
                for rule_list in rule_lists:
                    try:
                        values = json.loads(rule_list.list_values)
                        new_cache[rule_list.name] = set(values)
                        new_metadata[rule_list.name] = {
                            'id': rule_list.id,
                            'description': rule_list.description,
                            'data_type': rule_list.data_type,
                            'schema_version': rule_list.schema_version,
                            'created_at': rule_list.created_at,
                            'updated_at': rule_list.updated_at
                        }
                    except json.JSONDecodeError as e:
                        print(f"Warning: Invalid JSON in rule list '{rule_list.name}': {e}")
                        continue
                
                # Atomic update
                self._cache = new_cache
                self._metadata = new_metadata
                self._last_refresh = time.time()
                
                print(f"ListCache: Loaded {len(self._cache)} named lists")
                
        except Exception as e:
            print(f"Error loading lists from database: {e}")
            # Keep existing cache on error
    
    def _is_schema_compatible(self, list_schema: str, requested_schema: str) -> bool:
        """Check if list schema is compatible with requested schema."""
        if list_schema == 'both' or requested_schema == 'both':
            return True
        return list_schema == requested_schema
    
    def stats(self) -> Dict[str, Any]:
        """Get cache statistics."""
        with self._cache_lock:
            return {
                'cache_size': len(self._cache),
                'last_refresh': self._last_refresh,
                'refresh_interval': self._refresh_interval,
                'lists': list(self._cache.keys())
            }


class ListService:
    """Service for managing named rule lists."""
    
    def __init__(self):
        self.cache = ListCache()
    
    def get_list_values(self, name: str, schema_version: str = 'both') -> Optional[Set]:
        """Get list values for rule evaluation."""
        return self.cache.get_list(name, schema_version)
    
    def create_list(self, data: Dict[str, Any], created_by: str = 'system') -> RuleList:
        """Create a new named list."""
        rule_list = RuleList(
            name=data['name'],
            description=data.get('description', ''),
            data_type=data['data_type'],
            list_values=json.dumps(data['values']),
            schema_version=data.get('schema_version', 'both'),
            created_by=created_by,
            updated_by=created_by
        )
        
        db.session.add(rule_list)
        db.session.commit()
        
        # Invalidate cache to force refresh
        self.cache.invalidate(data['name'])
        
        return rule_list
    
    def update_list(self, list_id: int, data: Dict[str, Any], updated_by: str = 'system') -> Optional[RuleList]:
        """Update an existing named list."""
        rule_list = RuleList.query.get(list_id)
        if not rule_list:
            return None
        
        old_name = rule_list.name
        
        if 'name' in data:
            rule_list.name = data['name']
        if 'description' in data:
            rule_list.description = data['description']
        if 'data_type' in data:
            rule_list.data_type = data['data_type']
        if 'values' in data:
            rule_list.list_values = json.dumps(data['values'])
        if 'schema_version' in data:
            rule_list.schema_version = data['schema_version']
        
        rule_list.updated_by = updated_by
        
        db.session.commit()
        
        # Invalidate cache entries
        self.cache.invalidate(old_name)
        if old_name != rule_list.name:
            self.cache.invalidate(rule_list.name)
        
        return rule_list
    
    def delete_list(self, list_id: int) -> bool:
        """Delete a named list."""
        rule_list = RuleList.query.get(list_id)
        if not rule_list:
            return False
        
        name = rule_list.name
        db.session.delete(rule_list)
        db.session.commit()
        
        # Invalidate cache
        self.cache.invalidate(name)
        
        return True
    
    def get_all_lists(self, schema_version: str = 'both') -> Dict[str, Dict[str, Any]]:
        """Get all named lists."""
        return self.cache.get_all_lists(schema_version)
    
    def resolve_rule_lists(self, rule_content: str, schema_version: str = 'both') -> str:
        """
        Resolve named lists in rule content to actual values.
        
        Args:
            rule_content: Original rule content with named list references
            schema_version: Schema version for list compatibility
            
        Returns:
            Rule content with lists resolved to actual values
        """
        import re
        
        # Find all potential list references (IDENTIFIER not followed by . or ()
        pattern = r'\b([A-Z_][A-Z0-9_]*)\b(?!\s*[\.\(])'
        
        def replace_list_reference(match):
            list_name = match.group(1)
            list_values = self.get_list_values(list_name, schema_version)
            
            if list_values is not None:
                # Convert set to list and format as JSON-like array
                values_list = list(list_values)
                if all(isinstance(v, str) for v in values_list):
                    # String values need quotes
                    formatted_values = [f'"{v}"' for v in values_list]
                else:
                    # Numbers and booleans don't need quotes
                    formatted_values = [str(v) for v in values_list]
                
                return f'[{", ".join(formatted_values)}]'
            
            # Return original if not a named list
            return match.group(0)
        
        return re.sub(pattern, replace_list_reference, rule_content)