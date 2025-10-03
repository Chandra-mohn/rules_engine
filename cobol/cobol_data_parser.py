"""
COBOL Data Parser
Reads fixed-width ASCII data files using JSON schema from copybook parser.
"""

import json
from typing import Dict, Any, List, Optional, Callable
from pathlib import Path


class RedefinesStrategy:
    """Strategy for handling REDEFINES disambiguation"""

    @staticmethod
    def discriminator(record: Dict[str, Any], field_path: str, value_map: Dict[str, str]) -> Optional[str]:
        """
        Use a discriminator field to determine variant

        Args:
            record: Partially parsed record
            field_path: Path to discriminator field (e.g., 'TX-DATA.TX-TYPE')
            value_map: Map of field value to variant name
                      e.g., {'PAYMENT': 'PAYMENT-DATA', 'REFUND': 'REFUND-DATA'}

        Returns:
            Variant name or None
        """
        parts = field_path.split('.')
        current = record
        for part in parts:
            if part in current:
                current = current[part]
            else:
                return None

        return value_map.get(current)

    @staticmethod
    def default_variant(record: Dict[str, Any]) -> str:
        """Always use first variant"""
        return "default"

    @staticmethod
    def try_all(record: Dict[str, Any], variants: List[str]) -> str:
        """Try parsing with each variant, return first that validates"""
        # For now, return first variant
        # In production, would attempt parsing and validation
        return variants[0] if variants else "default"


class CobolDataParser:
    """Parses fixed-width ASCII data files using JSON schema"""

    def __init__(self, schema_file: str):
        """
        Initialize parser with JSON schema

        Args:
            schema_file: Path to JSON schema file (from json_schema_generator.py)
        """
        with open(schema_file, 'r') as f:
            schema_data = json.load(f)

        # Handle both single schema and multi-variant format
        if 'schemas' in schema_data:
            # Multi-variant format
            self.schemas = schema_data['schemas']
            self.primary_schema = self.schemas[0] if self.schemas else {}
        else:
            # Single schema format
            self.primary_schema = schema_data
            self.schemas = [schema_data]

        self.record_size = self.primary_schema.get('x-cobol-size', 0)
        self.redefines_strategy = None

    def set_redefines_strategy(self, strategy_fn: Callable):
        """
        Set REDEFINES disambiguation strategy

        Args:
            strategy_fn: Function that determines which variant to use
        """
        self.redefines_strategy = strategy_fn

    def parse_file(self, data_file: str, max_records: Optional[int] = None) -> List[Dict[str, Any]]:
        """
        Parse entire data file

        Args:
            data_file: Path to fixed-width data file
            max_records: Maximum records to parse (None = all)

        Returns:
            List of parsed records as dicts
        """
        records = []

        with open(data_file, 'r', encoding='ascii') as f:
            line_num = 0
            for line in f:
                line_num += 1

                # Remove newline
                line = line.rstrip('\n')

                # Skip empty lines
                if not line:
                    continue

                # Length validation
                if len(line) != self.record_size:
                    print(f"Warning: Line {line_num} length {len(line)} != expected {self.record_size}")

                # Parse record
                try:
                    record = self.parse_record(line)
                    records.append(record)
                except Exception as e:
                    print(f"Error parsing line {line_num}: {e}")
                    continue

                # Max records limit
                if max_records and len(records) >= max_records:
                    break

        return records

    def parse_record(self, data: str) -> Dict[str, Any]:
        """
        Parse a single record

        Args:
            data: Fixed-width data string

        Returns:
            Parsed record as dict
        """
        record = {}
        self._parse_properties(data, self.primary_schema['properties'], record, 0)
        return record

    def _parse_properties(self, data: str, properties: Dict[str, Any], result: Dict[str, Any], base_offset: int):
        """
        Parse properties recursively

        Args:
            data: Full record data
            properties: Schema properties dict
            result: Dict to populate with parsed values
            base_offset: Base offset for this group
        """
        for field_name, field_schema in properties.items():
            # Get field metadata
            offset = field_schema.get('x-cobol-offset', 0)
            size = field_schema.get('x-cobol-size', 0)
            field_type = field_schema.get('type')

            # Handle arrays (OCCURS)
            if field_type == 'array':
                result[field_name] = self._parse_array(data, field_schema, base_offset + offset)

            # Handle groups (nested objects)
            elif field_type == 'object':
                result[field_name] = {}
                self._parse_properties(
                    data,
                    field_schema.get('properties', {}),
                    result[field_name],
                    base_offset + offset
                )

            # Handle elementary items
            else:
                # Extract field data
                start = base_offset + offset
                end = start + size
                if end <= len(data):
                    field_data = data[start:end]
                    result[field_name] = self._convert_value(field_data, field_schema)
                else:
                    result[field_name] = None

    def _parse_array(self, data: str, array_schema: Dict[str, Any], base_offset: int) -> List[Any]:
        """
        Parse OCCURS array

        Args:
            data: Full record data
            array_schema: Array schema definition
            base_offset: Array start offset

        Returns:
            List of array elements
        """
        items_schema = array_schema.get('items', {})
        max_items = array_schema.get('maxItems', 1)
        item_size = items_schema.get('x-cobol-size', 0)

        result = []
        for i in range(max_items):
            item_offset = base_offset + (i * item_size)

            if items_schema.get('type') == 'object':
                # Array of groups
                item = {}
                self._parse_properties(
                    data,
                    items_schema.get('properties', {}),
                    item,
                    item_offset
                )
                result.append(item)
            else:
                # Array of elementary items
                start = item_offset
                end = start + item_size
                if end <= len(data):
                    field_data = data[start:end]
                    result.append(self._convert_value(field_data, items_schema))

        return result

    def _convert_value(self, data: str, schema: Dict[str, Any]) -> Any:
        """
        Convert ASCII field data to Python type

        Args:
            data: ASCII field data
            schema: Field schema

        Returns:
            Converted value
        """
        field_type = schema.get('type', 'string')

        # Strip padding
        data = data.strip()

        if not data:
            return None

        # Type conversion (best effort)
        try:
            if field_type == 'integer':
                return int(data)
            elif field_type == 'number':
                return float(data)
            elif field_type == 'string':
                return data
            else:
                return data
        except ValueError:
            # Best effort - return as string if conversion fails
            return data


def create_sample_data(schema_file: str, output_file: str, num_records: int = 10):
    """
    Create sample data file from schema

    Args:
        schema_file: JSON schema file
        output_file: Output data file path
        num_records: Number of sample records to generate
    """
    parser = CobolDataParser(schema_file)
    schema = parser.primary_schema

    with open(output_file, 'w', encoding='ascii') as f:
        for record_num in range(num_records):
            record_data = _generate_sample_record(schema, record_num)
            f.write(record_data + '\n')

    print(f"Generated {num_records} sample records to {output_file}")
    print(f"Record size: {len(record_data)} bytes")


def _generate_sample_record(schema: Dict[str, Any], record_num: int) -> str:
    """Generate a single sample record"""
    record_size = schema.get('x-cobol-size', 100)
    record = [' '] * record_size

    _fill_properties(schema.get('properties', {}), record, record_num, 0)

    return ''.join(record)


def _fill_properties(properties: Dict[str, Any], record: List[str], record_num: int, base_offset: int):
    """Fill record with sample data"""
    for field_name, field_schema in properties.items():
        offset = field_schema.get('x-cobol-offset', 0)
        size = field_schema.get('x-cobol-size', 0)
        field_type = field_schema.get('type')

        # Handle arrays
        if field_type == 'array':
            items_schema = field_schema.get('items', {})
            max_items = field_schema.get('maxItems', 1)
            item_size = items_schema.get('x-cobol-size', 0)

            for i in range(max_items):
                item_offset = base_offset + offset + (i * item_size)
                if items_schema.get('type') == 'object':
                    _fill_properties(items_schema.get('properties', {}), record, record_num, item_offset)
                else:
                    value = _sample_value(field_name, items_schema, record_num, i)
                    _write_field(record, item_offset, size, value)

        # Handle groups
        elif field_type == 'object':
            _fill_properties(field_schema.get('properties', {}), record, record_num, base_offset + offset)

        # Handle elementary items
        else:
            value = _sample_value(field_name, field_schema, record_num, 0)
            _write_field(record, base_offset + offset, size, value)


def _sample_value(field_name: str, schema: Dict[str, Any], record_num: int, array_idx: int) -> str:
    """Generate sample value based on field name and type"""
    field_type = schema.get('type', 'string')
    size = schema.get('x-cobol-size', 10)

    # Generate context-aware sample data
    name_lower = field_name.lower()

    if 'id' in name_lower or 'number' in name_lower:
        if field_type == 'integer':
            return str(1000000000 + record_num).zfill(size)
        return str(record_num + 1).zfill(size)

    elif 'date' in name_lower:
        return '20250101'[:size].ljust(size)

    elif 'time' in name_lower:
        return '120000'[:size].ljust(size)

    elif 'amount' in name_lower or 'price' in name_lower or 'total' in name_lower:
        if field_type in ('integer', 'number'):
            return str(100 + record_num * 10).zfill(size)
        return str(100.00 + record_num * 10.0)[:size].ljust(size)

    elif 'name' in name_lower:
        names = ['John', 'Jane', 'Alice', 'Bob', 'Charlie', 'David', 'Eve', 'Frank']
        return names[record_num % len(names)][:size].ljust(size)

    elif 'type' in name_lower or 'status' in name_lower:
        types = ['01', '02', '03', 'A', 'B', 'C', 'X', 'Y']
        return types[record_num % len(types)][:size].ljust(size)

    elif 'code' in name_lower:
        return f'CD{record_num:03d}'[:size].ljust(size)

    elif 'email' in name_lower:
        return f'user{record_num}@example.com'[:size].ljust(size)

    elif 'phone' in name_lower:
        return f'555-{1000+record_num:04d}'[:size].ljust(size)

    elif field_type == 'integer':
        return str(record_num).zfill(size)

    elif field_type == 'number':
        return str(float(record_num))[:size].ljust(size)

    else:
        return f'DATA{record_num}'[:size].ljust(size)


def _write_field(record: List[str], offset: int, size: int, value: str):
    """Write field value to record at offset"""
    # Ensure value fits
    value = str(value)[:size].ljust(size)

    for i, char in enumerate(value):
        if offset + i < len(record):
            record[offset + i] = char


if __name__ == '__main__':
    # Demo usage
    import sys

    if len(sys.argv) < 2:
        print("Usage:")
        print("  Generate sample data: python cobol_data_parser.py generate <schema.json> <output.dat>")
        print("  Parse data: python cobol_data_parser.py parse <schema.json> <data.dat>")
        sys.exit(1)

    command = sys.argv[1]

    if command == 'generate':
        if len(sys.argv) < 4:
            print("Usage: python cobol_data_parser.py generate <schema.json> <output.dat> [num_records]")
            sys.exit(1)

        schema_file = sys.argv[2]
        output_file = sys.argv[3]
        num_records = int(sys.argv[4]) if len(sys.argv) > 4 else 10

        create_sample_data(schema_file, output_file, num_records)

    elif command == 'parse':
        if len(sys.argv) < 4:
            print("Usage: python cobol_data_parser.py parse <schema.json> <data.dat>")
            sys.exit(1)

        schema_file = sys.argv[2]
        data_file = sys.argv[3]

        parser = CobolDataParser(schema_file)
        records = parser.parse_file(data_file, max_records=5)

        print(f"\nParsed {len(records)} records:")
        for i, record in enumerate(records, 1):
            print(f"\n--- Record {i} ---")
            print(json.dumps(record, indent=2))

    else:
        print(f"Unknown command: {command}")
        sys.exit(1)
