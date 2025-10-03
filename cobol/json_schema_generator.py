"""
JSON Schema Generator
Generates JSON schema from processed COBOL copybook AST.
"""

import json
from typing import Dict, Any, List, Optional
from pathlib import Path
from copybook_parser import ASTNode, NodeType
from copybook_processor import CopybookProcessor, RedefinesEnumerator, ProcessedCopybook


class JSONSchemaGenerator:
    """Generates JSON schema from COBOL copybook AST"""

    def __init__(self, processed: ProcessedCopybook):
        """
        Initialize generator

        Args:
            processed: ProcessedCopybook from CopybookProcessor
        """
        self.processed = processed

    def generate_schema(self, node: ASTNode, include_metadata: bool = True) -> Dict[str, Any]:
        """
        Generate JSON schema for an AST node

        Args:
            node: AST node to generate schema for
            include_metadata: Include COBOL-specific metadata

        Returns:
            JSON schema dictionary
        """
        schema = {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "title": node.name,
            "type": "object"
        }

        if include_metadata:
            schema["description"] = f"Generated from COBOL copybook level {node.level:02d} {node.name}"
            schema["x-cobol-level"] = node.level
            schema["x-cobol-offset"] = node.offset
            schema["x-cobol-size"] = node.storage_size

        # Generate properties
        properties, required = self._generate_properties(node, include_metadata)
        schema["properties"] = properties

        if required:
            schema["required"] = required

        return schema

    def _generate_properties(self, node: ASTNode, include_metadata: bool) -> tuple[Dict[str, Any], List[str]]:
        """
        Generate properties for a node's children

        Args:
            node: Parent node
            include_metadata: Include COBOL-specific metadata

        Returns:
            Tuple of (properties dict, required fields list)
        """
        properties = {}
        required = []

        for child in node.children:
            prop = self._node_to_property(child, include_metadata)
            properties[child.name] = prop

            # Mark as required (all COBOL fields are typically required)
            required.append(child.name)

        return properties, required

    def _node_to_property(self, node: ASTNode, include_metadata: bool) -> Dict[str, Any]:
        """
        Convert an AST node to a JSON schema property

        Args:
            node: AST node
            include_metadata: Include COBOL-specific metadata

        Returns:
            JSON schema property dictionary
        """
        prop: Dict[str, Any] = {}

        # Treat REDEFINES nodes with children as groups
        if node.children:
            # Group item (including REDEFINES groups with children)
            prop = self._group_to_property(node, include_metadata)
        elif node.is_elementary():
            # Elementary item - map to JSON type
            prop = self._elementary_to_property(node)

        # Add COBOL metadata
        if include_metadata:
            prop["x-cobol-level"] = node.level
            prop["x-cobol-offset"] = node.offset
            prop["x-cobol-size"] = node.storage_size

            if node.picture:
                prop["x-cobol-picture"] = node.picture.picture_string

            if node.usage:
                prop["x-cobol-usage"] = node.usage.usage_type

            if node.sign_clause:
                prop["x-cobol-sign"] = node.sign_clause
                prop["x-cobol-sign-separate"] = node.sign_separate

            if node.value:
                prop["x-cobol-value"] = node.value

            if node.blank_when_zero:
                prop["x-cobol-blank-when-zero"] = True

            if node.justified:
                prop["x-cobol-justified"] = "right" if node.justified_right else True

        # Handle OCCURS clause
        if node.occurs:
            # Wrap in array
            array_prop = {
                "type": "array",
                "items": prop,
                "minItems": node.occurs,
                "maxItems": node.occurs
            }
            if include_metadata:
                array_prop["x-cobol-occurs"] = node.occurs
            return array_prop

        return prop

    def _elementary_to_property(self, node: ASTNode) -> Dict[str, Any]:
        """Convert elementary item to JSON schema property"""
        prop = {}

        if not node.picture:
            prop["type"] = "string"
            return prop

        # Determine JSON type based on COBOL picture
        if node.picture.data_type == 'numeric':
            # Numeric field
            if 'V' in node.picture.picture_string.upper():
                # Has decimal point - use number
                prop["type"] = "number"
            else:
                # Integer
                prop["type"] = "integer"

            # Add constraints based on picture
            digits = node.picture.display_length
            if 'S' in node.picture.picture_string.upper():
                # Signed
                prop["minimum"] = -(10 ** digits - 1)
                prop["maximum"] = 10 ** digits - 1
            else:
                # Unsigned
                prop["minimum"] = 0
                prop["maximum"] = 10 ** digits - 1

        elif node.picture.data_type in ('alphanumeric', 'alphabetic'):
            # String field
            prop["type"] = "string"
            prop["maxLength"] = node.picture.display_length

        else:
            # Default to string
            prop["type"] = "string"

        return prop

    def _group_to_property(self, node: ASTNode, include_metadata: bool) -> Dict[str, Any]:
        """Convert group item to JSON schema property"""
        prop = {
            "type": "object"
        }

        # Generate nested properties
        properties, required = self._generate_properties(node, include_metadata)
        prop["properties"] = properties

        if required:
            prop["required"] = required

        return prop


class MultiFileSchemaGenerator:
    """Generates multiple JSON schema files for REDEFINES variants"""

    def __init__(self, copybook_path: str, output_dir: str = 'output'):
        """
        Initialize multi-file generator

        Args:
            copybook_path: Path to COBOL copybook file
            output_dir: Output directory for JSON schema files
        """
        self.copybook_path = copybook_path
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)
        self.max_variants = 10000
        self.max_depth = None
        self.enum_mode = 'hierarchical'

    def generate(self, include_metadata: bool = True) -> List[str]:
        """
        Generate JSON schema files for all REDEFINES variants

        Args:
            include_metadata: Include COBOL-specific metadata

        Returns:
            List of generated file paths
        """
        from copybook_parser import parse_copybook
        from copybook_processor import HierarchicalRedefinesEnumerator

        # Parse and process copybook
        ast = parse_copybook(self.copybook_path)
        processor = CopybookProcessor(ast)
        processed = processor.process()

        num_redefines = len(processed.redefines_groups)
        print(f"Detected {num_redefines} REDEFINES groups")

        # Choose enumeration strategy
        if self.enum_mode == 'hierarchical' or num_redefines > 20:
            if num_redefines > 20:
                print(f"Large copybook detected: Using hierarchical enumeration")
                print(f"Mode: hierarchical, Max variants: {self.max_variants}, Max depth: {self.max_depth or 'unlimited'}")

            # Use hierarchical enumerator with streaming
            enumerator = HierarchicalRedefinesEnumerator(
                processed,
                max_variants=self.max_variants,
                max_depth=self.max_depth
            )

            # Process variants in streaming fashion
            variants = []
            grouped: Dict[str, List] = {}

            print(f"Generating schemas...")
            for i, (variant_name, variant_tree) in enumerate(enumerator.enumerate_variants_streaming(), 1):
                if i % 100 == 0:
                    print(f"  Generated {i} variants...")

                generator = JSONSchemaGenerator(processed)
                schema = generator.generate_schema(variant_tree, include_metadata)
                schema["x-variant-name"] = variant_name

                # Group by first part of variant name (top-level choice)
                parts = variant_name.split('_')
                top_level = parts[0] if parts else "default"

                if top_level not in grouped:
                    grouped[top_level] = []
                grouped[top_level].append((variant_name, schema))

            print(f"Total variants generated: {enumerator.variant_count}")

        else:
            # Use flat enumeration for small copybooks
            print(f"Using flat enumeration (mode: {self.enum_mode})")
            enumerator = RedefinesEnumerator(processed)
            variants = enumerator.enumerate_variants()

            # Group by top-level REDEFINES
            grouped_tuples = enumerator.group_variants_by_top_level(variants)

            # Convert to schema format
            grouped = {}
            for group_name, group_variants in grouped_tuples.items():
                schemas_list = []
                for variant_name, variant_tree in group_variants:
                    generator = JSONSchemaGenerator(processed)
                    schema = generator.generate_schema(variant_tree, include_metadata)
                    schema["x-variant-name"] = variant_name
                    schemas_list.append(schema)
                grouped[group_name] = schemas_list

        generated_files = []

        # Generate a file for each top-level group
        for group_name, group_schemas in grouped.items():
            # Write to file
            filename = f"{ast.name}_{group_name}.json"
            filepath = self.output_dir / filename

            with open(filepath, 'w', encoding='utf-8') as f:
                json.dump({
                    "schemas": group_schemas,
                    "copybook": ast.name,
                    "group": group_name,
                    "variant_count": len(group_schemas)
                }, f, indent=2)

            generated_files.append(str(filepath))
            print(f"Generated: {filepath} ({len(group_schemas)} variants)")

        return generated_files


class SingleSchemaGenerator:
    """Generates a single JSON schema file with all variants"""

    def __init__(self, copybook_path: str, output_file: str = None):
        """
        Initialize single-file generator

        Args:
            copybook_path: Path to COBOL copybook file
            output_file: Output JSON schema file path
        """
        self.copybook_path = copybook_path
        self.output_file = output_file
        self.max_variants = 10000
        self.max_depth = None
        self.enum_mode = 'hierarchical'

    def generate(self, include_metadata: bool = True) -> str:
        """
        Generate single JSON schema file with all REDEFINES variants

        Args:
            include_metadata: Include COBOL-specific metadata

        Returns:
            Generated file path
        """
        from copybook_parser import parse_copybook
        from copybook_processor import HierarchicalRedefinesEnumerator

        # Parse and process copybook
        ast = parse_copybook(self.copybook_path)
        processor = CopybookProcessor(ast)
        processed = processor.process()

        # Determine output file
        if not self.output_file:
            self.output_file = f"{ast.name}_schema.json"

        # Generate schema for main structure
        generator = JSONSchemaGenerator(processed)
        main_schema = generator.generate_schema(ast, include_metadata)

        # Generate all variants if there are REDEFINES
        variants = []
        num_redefines = len(processed.redefines_groups)

        if processed.redefines_groups:
            print(f"Detected {num_redefines} REDEFINES groups")

            # Choose enumeration strategy
            if self.enum_mode == 'hierarchical' or num_redefines > 20:
                if num_redefines > 20:
                    print(f"Large copybook detected: Using hierarchical enumeration")
                    print(f"Mode: hierarchical, Max variants: {self.max_variants}, Max depth: {self.max_depth or 'unlimited'}")

                # Use hierarchical enumerator with streaming
                enumerator = HierarchicalRedefinesEnumerator(
                    processed,
                    max_variants=self.max_variants,
                    max_depth=self.max_depth
                )

                print(f"Generating schemas...")
                for i, (variant_name, variant_tree) in enumerate(enumerator.enumerate_variants_streaming(), 1):
                    if i % 100 == 0:
                        print(f"  Generated {i} variants...")

                    variant_schema = generator.generate_schema(variant_tree, include_metadata)
                    variant_schema["x-variant-name"] = variant_name
                    variants.append(variant_schema)

                print(f"Total variants generated: {enumerator.variant_count}")

            else:
                # Use flat enumeration for small copybooks
                print(f"Using flat enumeration (mode: {self.enum_mode})")
                enumerator = RedefinesEnumerator(processed)
                variant_list = enumerator.enumerate_variants()

                for variant_name, variant_tree in variant_list:
                    variant_schema = generator.generate_schema(variant_tree, include_metadata)
                    variant_schema["x-variant-name"] = variant_name
                    variants.append(variant_schema)

        # Combine into single output
        output = {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "title": f"{ast.name} - COBOL Copybook Schema",
            "main_schema": main_schema,
            "variants": variants if variants else None,
            "metadata": {
                "copybook_name": ast.name,
                "total_size": processed.total_size,
                "redefines_groups": len(processed.redefines_groups),
                "variant_count": len(variants)
            }
        }

        # Write to file
        with open(self.output_file, 'w', encoding='utf-8') as f:
            json.dump(output, f, indent=2)

        print(f"Generated: {self.output_file}")
        print(f"  - Total size: {processed.total_size} bytes")
        print(f"  - REDEFINES groups: {len(processed.redefines_groups)}")
        print(f"  - Variants: {len(variants)}")

        return self.output_file


if __name__ == '__main__':
    import sys
    import argparse

    parser = argparse.ArgumentParser(description='Generate JSON schema from COBOL copybook')
    parser.add_argument('copybook', help='Path to COBOL copybook file')
    parser.add_argument('--multi-file', action='store_true',
                       help='Generate separate files for each top-level REDEFINES group')
    parser.add_argument('--output-dir', default='output',
                       help='Output directory for multi-file mode (default: output)')
    parser.add_argument('--output', help='Output file for single-file mode')
    parser.add_argument('--no-metadata', action='store_true',
                       help='Exclude COBOL-specific metadata')
    parser.add_argument('--max-variants', type=int, default=10000,
                       help='Maximum number of variants to generate (default: 10000)')
    parser.add_argument('--max-depth', type=int, default=None,
                       help='Maximum REDEFINES nesting depth to enumerate (default: unlimited)')
    parser.add_argument('--mode', choices=['flat', 'hierarchical'], default='hierarchical',
                       help='Enumeration mode: flat (all combinations) or hierarchical (context-aware, default)')

    args = parser.parse_args()

    include_metadata = not args.no_metadata

    # Set enumeration mode based on arguments
    if args.multi_file:
        generator = MultiFileSchemaGenerator(args.copybook, args.output_dir)
        generator.max_variants = args.max_variants
        generator.max_depth = args.max_depth
        generator.enum_mode = args.mode
        files = generator.generate(include_metadata)
        print(f"\nGenerated {len(files)} file(s):")
        for f in files:
            print(f"  - {f}")
    else:
        generator = SingleSchemaGenerator(args.copybook, args.output)
        generator.max_variants = args.max_variants
        generator.max_depth = args.max_depth
        generator.enum_mode = args.mode
        output_file = generator.generate(include_metadata)
        print(f"\nSchema generated: {output_file}")
