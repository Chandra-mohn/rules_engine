"""
List Cache Service for Named Lists in Rules Engine
Provides lazy loading and caching for rule list definitions.
"""

import time
import json
import threading
from typing import Optional, Set, Dict, Any, List
from pathlib import Path
from datetime import datetime


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
        self._lists_path = Path(__file__).parent.parent / 'lists'
        self._lists_path.mkdir(exist_ok=True)
    
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
        """Load all lists from files into cache."""
        try:
            new_cache = {}
            new_metadata = {}

            # Load all JSON files from lists directory
            for file_path in self._lists_path.glob('*.json'):
                if file_path.name == 'schema.json':
                    continue

                try:
                    with open(file_path, 'r') as f:
                        list_data = json.load(f)

                    list_name = list_data['list_name']
                    values = list_data.get('values', [])

                    # Convert to set for fast membership testing
                    # Extract just the codes from value objects
                    value_codes = [v['code'] if isinstance(v, dict) else v for v in values]
                    new_cache[list_name] = set(value_codes)

                    new_metadata[list_name] = {
                        'description': list_data.get('description', ''),
                        'data_type': 'string',  # Default type
                        'schema_version': list_data.get('schema_version', 'both'),
                        'created_at': list_data.get('metadata', {}).get('created_at'),
                        'updated_at': list_data.get('metadata', {}).get('updated_at')
                    }
                except json.JSONDecodeError as e:
                    print(f"Warning: Invalid JSON in list file '{file_path.name}': {e}")
                    continue
                except Exception as e:
                    print(f"Warning: Error loading list from '{file_path.name}': {e}")
                    continue

            # Atomic update
            self._cache = new_cache
            self._metadata = new_metadata
            self._last_refresh = time.time()

            print(f"ListCache: Loaded {len(self._cache)} named lists from files")

        except Exception as e:
            print(f"Error loading lists from files: {e}")
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
        self.lists_path = Path(__file__).parent.parent / 'lists'
        self.lists_path.mkdir(exist_ok=True)

    def get_list_values(self, name: str, schema_version: str = 'both') -> Optional[Set]:
        """Get list values for rule evaluation."""
        return self.cache.get_list(name, schema_version)

    def create_list(self, data: Dict[str, Any], created_by: str = 'system') -> Dict[str, Any]:
        """Create a new named list."""
        list_data = {
            'list_name': data['name'],
            'display_name': data.get('display_name', data['name']),
            'description': data.get('description', ''),
            'schema_version': data.get('schema_version', 'both'),
            'values': data.get('values', []),
            'metadata': {
                'created_at': datetime.utcnow().isoformat(),
                'updated_at': datetime.utcnow().isoformat(),
                'created_by': created_by,
                'updated_by': created_by,
                'version': 1
            }
        }

        # Write to file
        file_path = self.lists_path / f"{data['name']}.json"
        with open(file_path, 'w') as f:
            json.dump(list_data, f, indent=2)

        # Invalidate cache to force refresh
        self.cache.invalidate(data['name'])

        return list_data
    
    def update_list(self, list_name: str, data: Dict[str, Any], updated_by: str = 'system') -> Optional[Dict[str, Any]]:
        """Update an existing named list."""
        file_path = self.lists_path / f"{list_name}.json"
        if not file_path.exists():
            return None

        # Read existing data
        with open(file_path, 'r') as f:
            list_data = json.load(f)

        old_name = list_data['list_name']

        # Update fields
        if 'name' in data:
            list_data['list_name'] = data['name']
        if 'display_name' in data:
            list_data['display_name'] = data['display_name']
        if 'description' in data:
            list_data['description'] = data['description']
        if 'values' in data:
            list_data['values'] = data['values']
        if 'schema_version' in data:
            list_data['schema_version'] = data['schema_version']

        # Update metadata
        list_data['metadata']['updated_at'] = datetime.utcnow().isoformat()
        list_data['metadata']['updated_by'] = updated_by
        list_data['metadata']['version'] = list_data['metadata'].get('version', 1) + 1

        # Handle name change
        if old_name != list_data['list_name']:
            # Delete old file
            file_path.unlink()
            # Create new file
            file_path = self.lists_path / f"{list_data['list_name']}.json"

        # Write updated data
        with open(file_path, 'w') as f:
            json.dump(list_data, f, indent=2)

        # Invalidate cache entries
        self.cache.invalidate(old_name)
        if old_name != list_data['list_name']:
            self.cache.invalidate(list_data['list_name'])

        return list_data
    
    def delete_list(self, list_name: str) -> bool:
        """Delete a named list."""
        file_path = self.lists_path / f"{list_name}.json"
        if not file_path.exists():
            return False

        # Delete file
        file_path.unlink()

        # Invalidate cache
        self.cache.invalidate(list_name)

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