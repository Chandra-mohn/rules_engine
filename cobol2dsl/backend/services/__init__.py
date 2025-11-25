"""
COBOL-to-DSL Migration Services

Provides services for attribute mapping, pattern detection, and code generation.
"""

from .mapping_service import MappingService, AttributeMapping, convert_to_camel_case

__all__ = [
    'MappingService',
    'AttributeMapping',
    'convert_to_camel_case',
]
