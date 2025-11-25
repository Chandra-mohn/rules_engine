"""
COBOL Attribute Mapping Service

Uses DuckDB to query CSV files for COBOL→target system attribute mappings.
Git-friendly storage with human-readable CSV files.
"""

import duckdb
from pathlib import Path
from typing import Optional, Dict, Any
from dataclasses import dataclass


@dataclass
class AttributeMapping:
    """Represents a COBOL attribute mapping"""
    cobol_name: str
    target_name: str
    mapping_type: str  # 'direct', 'fuzzy', 'action', 'temp'
    data_type: str     # 'string', 'integer', 'decimal', 'function'
    confidence: float   # 0.0 to 1.0


class MappingService:
    """
    Service for querying COBOL attribute mappings from CSV files via DuckDB.

    Usage:
        mapper = MappingService("mappings/attribute_mappings.csv")
        mapping = mapper.get_mapping("CUSTOMER-TYPE")
    """

    def __init__(self, csv_path: str = "mappings/sample_attribute_mappings.csv"):
        """
        Initialize mapping service with CSV file path.

        Args:
            csv_path: Path to CSV file containing mappings
        """
        self.csv_path = Path(csv_path)
        if not self.csv_path.exists():
            raise FileNotFoundError(f"Mapping CSV not found: {csv_path}")

        # In-memory DuckDB connection
        self.conn = duckdb.connect(':memory:')

        # Session cache for performance
        self.cache: Dict[str, Optional[AttributeMapping]] = {}

        # Load CSV as view - DuckDB reads it directly
        self.conn.execute(f"""
            CREATE VIEW v_attribute_mappings AS
            SELECT * FROM read_csv_auto('{self.csv_path.absolute()}')
        """)

    def get_mapping(self, cobol_name: str) -> Optional[AttributeMapping]:
        """
        Get attribute mapping for a COBOL variable name.

        Args:
            cobol_name: COBOL variable name (e.g., "CUSTOMER-TYPE")

        Returns:
            AttributeMapping if found, None otherwise
        """
        # Check cache first
        if cobol_name in self.cache:
            return self.cache[cobol_name]

        # Query DuckDB view
        query = """
            SELECT cobol_name, target_name, mapping_type, data_type, confidence
            FROM v_attribute_mappings
            WHERE cobol_name = ?
        """

        result = self.conn.execute(query, [cobol_name]).fetchone()

        if result:
            mapping = AttributeMapping(
                cobol_name=result[0],
                target_name=result[1],
                mapping_type=result[2],
                data_type=result[3],
                confidence=float(result[4])
            )
            self.cache[cobol_name] = mapping
            return mapping

        # Cache negative results to avoid repeated queries
        self.cache[cobol_name] = None
        return None

    def get_all_mappings(self) -> list[AttributeMapping]:
        """
        Get all available mappings.

        Returns:
            List of all AttributeMapping objects
        """
        query = "SELECT * FROM v_attribute_mappings"
        results = self.conn.execute(query).fetchall()

        return [
            AttributeMapping(
                cobol_name=row[0],
                target_name=row[1],
                mapping_type=row[2],
                data_type=row[3],
                confidence=float(row[4])
            )
            for row in results
        ]

    def derive_mapping(self, cobol_name: str) -> AttributeMapping:
        """
        Derive a mapping for unmapped COBOL variables.
        Converts COBOL naming conventions to modern naming.

        Args:
            cobol_name: COBOL variable name

        Returns:
            Derived AttributeMapping with lower confidence
        """
        # Convert COBOL naming to camelCase
        parts = cobol_name.lower().split('-')

        # Simple entity detection (CUST, ACCT, etc.)
        entity_map = {
            'cust': 'customer',
            'acct': 'account',
            'app': 'application',
            'trans': 'transaction',
        }

        entity = entity_map.get(parts[0], parts[0])

        if len(parts) > 1:
            # e.g., CUSTOMER-TYPE → customer.type
            attribute = '_'.join(parts[1:])
            target_name = f"{entity}.{attribute}"
        else:
            # Single word
            target_name = entity

        return AttributeMapping(
            cobol_name=cobol_name,
            target_name=target_name,
            mapping_type='derived',
            data_type='unknown',
            confidence=0.5
        )

    def map_with_fallback(self, cobol_name: str) -> AttributeMapping:
        """
        Get mapping with automatic fallback to derivation.

        Args:
            cobol_name: COBOL variable name

        Returns:
            AttributeMapping (either from CSV or derived)
        """
        mapping = self.get_mapping(cobol_name)
        if mapping:
            return mapping

        # Fallback to derivation
        return self.derive_mapping(cobol_name)

    def close(self):
        """Close DuckDB connection"""
        if self.conn:
            self.conn.close()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()


def convert_to_camel_case(cobol_name: str) -> str:
    """
    Convert COBOL naming convention to camelCase.

    Examples:
        APPROVE-PREMIUM → approvePremium
        WORK-TOTAL-AMT → workTotalAmt

    Args:
        cobol_name: COBOL variable name

    Returns:
        camelCase version
    """
    parts = cobol_name.lower().replace('_', '-').split('-')
    if not parts:
        return cobol_name.lower()

    return parts[0] + ''.join(p.capitalize() for p in parts[1:])
