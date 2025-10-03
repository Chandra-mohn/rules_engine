#!/usr/bin/env python3
"""
COBOL Copybook HTML Visualizer
Generates interactive HTML documentation from JSON schema files.
"""

import json
import sys
import argparse
import webbrowser
from pathlib import Path
from typing import Dict, Any, List, Tuple


class CopybookHTMLGenerator:
    """Generates HTML visualization from COBOL copybook JSON schema"""

    def __init__(self, schema_file: str, enable_comparison: bool = False):
        """
        Initialize generator with schema file

        Args:
            schema_file: Path to JSON schema file
            enable_comparison: Enable variant comparison feature
        """
        with open(schema_file, 'r') as f:
            schema_data = json.load(f)

        # Handle both single schema and multi-variant format
        if 'schemas' in schema_data:
            self.schemas = schema_data['schemas']
            self.is_multi_variant = True
        else:
            self.schemas = [schema_data]
            self.is_multi_variant = False

        self.schema_file = schema_file
        self.html_parts = []
        self.enable_comparison = enable_comparison

    def generate(self) -> str:
        """
        Generate complete HTML visualization

        Returns:
            Complete HTML document as string
        """
        self.html_parts = []

        self.html_parts.append(self._html_header())
        self.html_parts.append(self._styles())

        # Schema header with file info
        self.html_parts.append(self._schema_header())

        # Generate table for each variant
        for idx, schema in enumerate(self.schemas):
            variant_name = self._get_variant_name(schema, idx)
            self.html_parts.append(
                self._generate_variant_section(schema, variant_name, idx)
            )

        # Add comparison section if enabled and multiple variants exist
        if self.enable_comparison and len(self.schemas) > 1:
            self.html_parts.append(self._generate_comparison_section())

        self.html_parts.append(self._javascript())
        self.html_parts.append(self._html_footer())

        return '\n'.join(self.html_parts)

    def _get_variant_name(self, schema: Dict, idx: int) -> str:
        """Extract variant name from schema description or generate default"""
        description = schema.get('description', '')
        if 'variant:' in description.lower():
            # Extract variant name from description
            parts = description.split('variant:')
            if len(parts) > 1:
                return parts[1].strip().split()[0]
        return f"Variant {idx + 1}"

    def _html_header(self) -> str:
        """Generate HTML document header"""
        schema_name = Path(self.schema_file).stem
        return f'''<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{schema_name} - Copybook Visualization</title>
'''

    def _styles(self) -> str:
        """Generate CSS styles"""
        return '''    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: #f5f7fa;
            padding: 20px;
            line-height: 1.6;
        }

        .container {
            max-width: 100%;
            margin: 0 auto;
            background: white;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            padding: 30px;
        }

        h1 {
            color: #2c3e50;
            margin-bottom: 10px;
            font-size: 2em;
        }

        .schema-info {
            color: #7f8c8d;
            margin-bottom: 30px;
            padding: 15px;
            background: #ecf0f1;
            border-radius: 4px;
        }

        .schema-info strong {
            color: #2c3e50;
        }

        .variant-section {
            margin-bottom: 40px;
        }

        .variant-header {
            background: #3498db;
            color: white;
            padding: 15px 20px;
            border-radius: 4px;
            margin-bottom: 20px;
            font-size: 1.2em;
            font-weight: bold;
        }

        table.copybook {
            width: 100%;
            border-collapse: collapse;
            font-family: 'Courier New', Consolas, monospace;
            font-size: 0.9em;
            margin-bottom: 20px;
        }

        table.copybook thead {
            background: #2c3e50;
            color: white;
        }

        table.copybook th {
            text-align: left;
            padding: 12px 10px;
            font-weight: 600;
        }

        table.copybook td {
            padding: 8px 10px;
            border-bottom: 1px solid #ecf0f1;
        }

        table.copybook tbody tr:hover {
            background: #f8f9fa;
        }

        .field-name {
            white-space: nowrap;
        }

        .indent-0 { margin-left: 0em; }
        .indent-1 { margin-left: 2em; }
        .indent-2 { margin-left: 4em; }
        .indent-3 { margin-left: 6em; }
        .indent-4 { margin-left: 8em; }
        .indent-5 { margin-left: 10em; }

        .tree-icon {
            color: #95a5a6;
            margin-right: 4px;
        }

        .badge {
            display: inline-block;
            padding: 3px 8px;
            border-radius: 3px;
            font-size: 0.75em;
            font-weight: bold;
            margin-left: 8px;
            color: white;
        }

        .badge-redefines {
            background: #e74c3c;
        }

        .badge-occurs {
            background: #f39c12;
        }

        .badge-group {
            background: #9b59b6;
        }

        .redefines-header {
            background: #fff3cd !important;
        }

        .variant-label {
            background: #d1ecf1 !important;
            font-weight: bold;
            color: #0c5460;
            padding: 10px !important;
        }

        .variant-row {
            background: #f8f9fa;
        }

        .copy-btn {
            cursor: pointer;
            background: #3498db;
            color: white;
            border: none;
            padding: 5px 10px;
            border-radius: 3px;
            font-size: 0.8em;
            transition: background 0.3s;
        }

        .copy-btn:hover {
            background: #2980b9;
        }

        .copy-btn.copied {
            background: #27ae60;
        }

        .type-cell {
            font-weight: 600;
            color: #2c3e50;
        }

        .offset-cell {
            color: #7f8c8d;
            font-family: 'Courier New', monospace;
        }

        footer {
            margin-top: 40px;
            padding-top: 20px;
            border-top: 2px solid #ecf0f1;
            color: #7f8c8d;
            text-align: center;
        }

        /* Comparison Section Styles */
        .comparison-section {
            margin-top: 50px;
            padding: 30px;
            background: #f8f9fa;
            border-radius: 8px;
            border: 2px solid #dee2e6;
        }

        .comparison-section h2 {
            color: #2c3e50;
            margin-bottom: 20px;
            font-size: 1.5em;
        }

        .comparison-controls {
            display: flex;
            align-items: center;
            gap: 15px;
            margin-bottom: 20px;
            padding: 20px;
            background: white;
            border-radius: 4px;
        }

        .comparison-controls label {
            font-weight: 600;
            color: #2c3e50;
        }

        .variant-select {
            padding: 8px 12px;
            border: 1px solid #ced4da;
            border-radius: 4px;
            font-size: 0.9em;
            min-width: 200px;
        }

        .compare-btn {
            padding: 10px 20px;
            background: #27ae60;
            color: white;
            border: none;
            border-radius: 4px;
            cursor: pointer;
            font-weight: 600;
            transition: background 0.3s;
        }

        .compare-btn:hover {
            background: #229954;
        }

        .comparison-results {
            background: white;
            border-radius: 4px;
            padding: 20px;
            min-height: 200px;
        }

        .comparison-placeholder {
            text-align: center;
            color: #7f8c8d;
            font-style: italic;
            padding: 50px;
        }

        .comparison-summary {
            padding: 15px;
            background: #e8f4f8;
            border-left: 4px solid #3498db;
            margin-bottom: 20px;
            border-radius: 4px;
        }

        .comparison-summary h3 {
            margin-bottom: 10px;
            color: #2c3e50;
        }

        .comparison-summary ul {
            margin: 10px 0;
            padding-left: 20px;
        }

        .comparison-table {
            width: 100%;
            border-collapse: collapse;
            margin-top: 20px;
        }

        .comparison-table th {
            background: #2c3e50;
            color: white;
            text-align: left;
            padding: 12px;
            font-weight: 600;
        }

        .comparison-table td {
            padding: 10px;
            border-bottom: 1px solid #dee2e6;
        }

        .comparison-table tr:hover {
            background: #f1f3f5;
        }

        .diff-common {
            background: #d4edda !important;
            border-left: 4px solid #28a745;
        }

        .diff-v1-only {
            background: #fff3cd !important;
            border-left: 4px solid #ffc107;
        }

        .diff-v2-only {
            background: #cce5ff !important;
            border-left: 4px solid #0066cc;
        }

        .diff-type-diff {
            background: #f8d7da !important;
            border-left: 4px solid #dc3545;
        }

        .diff-badge {
            display: inline-block;
            padding: 3px 8px;
            border-radius: 3px;
            font-size: 0.75em;
            font-weight: bold;
            margin-left: 8px;
        }

        .badge-common {
            background: #28a745;
            color: white;
        }

        .badge-v1-only {
            background: #ffc107;
            color: #000;
        }

        .badge-v2-only {
            background: #0066cc;
            color: white;
        }

        .badge-different {
            background: #dc3545;
            color: white;
        }

        /* Side-by-Side Comparison Styles */
        .side-by-side-container {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 20px;
            margin-top: 20px;
            overflow-x: auto;
        }

        .variant-column {
            background: white;
            border-radius: 4px;
            padding: 15px;
            overflow-x: auto;
        }

        .variant-column h3 {
            position: sticky;
            top: 0;
            background: #3498db;
            color: white;
            padding: 10px 15px;
            margin: -15px -15px 15px -15px;
            border-radius: 4px 4px 0 0;
            z-index: 10;
        }

        .comparison-table-side {
            width: 100%;
            border-collapse: collapse;
            font-family: 'Courier New', Consolas, monospace;
            font-size: 0.68em;
        }

        .comparison-table-side th {
            background: #2c3e50;
            color: white;
            text-align: left;
            padding: 10px 8px;
            font-weight: 600;
            position: sticky;
            top: 45px;
            z-index: 5;
        }

        .comparison-table-side td {
            padding: 6px 8px;
            border-bottom: 1px solid #ecf0f1;
        }

        .comparison-table-side tbody tr:hover {
            background: #f8f9fa;
        }

        .field-absent {
            opacity: 0.3;
            font-style: italic;
            color: #95a5a6;
        }

        .field-present {
            opacity: 1;
        }

        /* Highlight matching rows on hover */
        .comparison-table-side tbody tr.highlight-row {
            background: #ffffcc !important;
        }

        @media (max-width: 1400px) {
            .side-by-side-container {
                grid-template-columns: 1fr;
            }

            .variant-column {
                margin-bottom: 20px;
            }
        }
    </style>
</head>
<body>
    <div class="container">
'''

    def _schema_header(self) -> str:
        """Generate schema information header"""
        schema_name = Path(self.schema_file).stem
        first_schema = self.schemas[0]
        total_size = first_schema.get('x-cobol-size', 0)
        variant_count = len(self.schemas)

        comparison_link = ''
        if self.enable_comparison and variant_count > 1:
            comparison_link = '<br><strong>📊 <a href="#comparison-section" style="color: #27ae60;">Jump to Variant Comparison</a></strong>'

        return f'''        <h1>{first_schema.get('title', schema_name)}</h1>
        <div class="schema-info">
            <strong>Schema File:</strong> {self.schema_file}<br>
            <strong>Record Size:</strong> {total_size} bytes<br>
            <strong>Variants:</strong> {variant_count} {'(multi-variant file)' if self.is_multi_variant else '(single variant)'}{comparison_link}
        </div>
'''

    def _generate_variant_section(self, schema: Dict, variant_name: str, idx: int) -> str:
        """Generate HTML table for one variant"""
        section_html = []

        # Variant header
        if self.is_multi_variant or len(self.schemas) > 1:
            section_html.append(f'''        <div class="variant-section">
            <div class="variant-header">
                {variant_name}
            </div>
''')
        else:
            section_html.append('        <div class="variant-section">\n')

        # Table
        section_html.append('''            <table class="copybook">
                <thead>
                    <tr>
                        <th>Level</th>
                        <th>Field Path</th>
                        <th>Type</th>
                        <th>Size</th>
                        <th>Offset</th>
                        <th>Action</th>
                    </tr>
                </thead>
                <tbody>
''')

        # Process fields
        properties = schema.get('properties', {})
        for field_name, field_schema in properties.items():
            rows = self._process_field(field_name, field_schema, level=0, path="")
            section_html.extend(rows)

        section_html.append('''                </tbody>
            </table>
        </div>
''')

        return '\n'.join(section_html)

    def _process_field(self, name: str, schema: Dict, level: int,
                      path: str, parent_offset: int = 0) -> List[str]:
        """
        Recursively process field and generate HTML rows

        Args:
            name: Field name
            schema: Field schema definition
            level: Nesting level (0-based)
            path: Dot-notation path to field
            parent_offset: Parent group offset

        Returns:
            List of HTML table row strings
        """
        rows = []

        field_type = schema.get('type')
        field_offset = schema.get('x-cobol-offset', 0) + parent_offset
        field_size = schema.get('x-cobol-size', 0)
        cobol_level = schema.get('x-cobol-level', 1)

        # Build field path
        current_path = f"{path}.{name}" if path else name

        # Check for REDEFINES
        redefines = schema.get('x-cobol-redefines')
        oneOf = schema.get('oneOf', [])

        if field_type == 'object' and not oneOf:
            # Regular group item
            rows.append(self._format_group_row(
                name, schema, level, current_path, field_offset, field_size, cobol_level
            ))

            # Process children
            for child_name, child_schema in schema.get('properties', {}).items():
                rows.extend(
                    self._process_field(child_name, child_schema, level + 1,
                                       current_path, field_offset)
                )

        elif oneOf:
            # REDEFINES group - show all variants
            rows.append(self._format_redefines_header(
                name, schema, level, current_path, field_offset, field_size, cobol_level
            ))

            # Process each variant
            for variant_idx, variant_schema in enumerate(oneOf):
                variant_name = self._extract_variant_name(variant_schema)

                # Variant label row
                rows.append(self._format_variant_label(variant_name, variant_idx, level))

                # Process variant properties
                variant_props = variant_schema.get('properties', {})
                for var_field_name, var_field_schema in variant_props.items():
                    rows.extend(
                        self._process_field(var_field_name, var_field_schema,
                                          level + 1, current_path, field_offset)
                    )

        elif field_type == 'array':
            # OCCURS clause
            occurs_count = schema.get('maxItems', 1)
            rows.append(self._format_occurs_row(
                name, schema, level, current_path, field_offset,
                field_size, cobol_level, occurs_count
            ))

            # Process array element structure (show [0] only)
            items_schema = schema.get('items', {})
            element_name = f"[0]"

            if items_schema.get('type') == 'object':
                # Array of groups - show structure
                for item_field_name, item_field_schema in items_schema.get('properties', {}).items():
                    rows.extend(
                        self._process_field(f"[0] {item_field_name}", item_field_schema,
                                          level + 1, f"{current_path}[0]", field_offset)
                    )
            else:
                # Array of elementary items
                rows.append(self._format_elementary_row(
                    element_name, items_schema, level + 1,
                    f"{current_path}[0]", field_offset
                ))

        else:
            # Elementary item
            rows.append(self._format_elementary_row(
                name, schema, level, current_path, field_offset
            ))

        return rows

    def _extract_variant_name(self, variant_schema: Dict) -> str:
        """Extract variant name from schema properties"""
        # Try to find the variant name from properties
        props = variant_schema.get('properties', {})
        if props:
            # Return the first property name as variant identifier
            return list(props.keys())[0]
        return "Unknown"

    def _format_group_row(self, name: str, schema: Dict, level: int,
                         path: str, offset: int, size: int, cobol_level: int) -> str:
        """Format HTML row for group item"""
        indent = f'indent-{level}'
        tree_icon = '├─' if level > 0 else ''

        return f'''                    <tr>
                        <td>{cobol_level:02d}</td>
                        <td class="field-name">
                            <span class="{indent}"><span class="tree-icon">{tree_icon}</span>{name}</span>
                            <span class="badge badge-group">GROUP</span>
                        </td>
                        <td class="type-cell">GROUP</td>
                        <td>{size}</td>
                        <td class="offset-cell">{offset}</td>
                        <td><button class="copy-btn" data-path="{path}">📋 Copy Path</button></td>
                    </tr>
'''

    def _format_redefines_header(self, name: str, schema: Dict, level: int,
                                path: str, offset: int, size: int, cobol_level: int) -> str:
        """Format HTML row for REDEFINES group header"""
        indent = f'indent-{level}'
        tree_icon = '├─' if level > 0 else ''
        variant_count = len(schema.get('oneOf', []))

        return f'''                    <tr class="redefines-header">
                        <td>{cobol_level:02d}</td>
                        <td class="field-name">
                            <span class="{indent}"><span class="tree-icon">{tree_icon}</span>{name}</span>
                            <span class="badge badge-redefines">REDEFINES ({variant_count} variants)</span>
                        </td>
                        <td class="type-cell">GROUP</td>
                        <td>{size}</td>
                        <td class="offset-cell">{offset}</td>
                        <td></td>
                    </tr>
'''

    def _format_variant_label(self, variant_name: str, idx: int, level: int) -> str:
        """Format variant separator row"""
        return f'''                    <tr class="variant-label">
                        <td colspan="6">
                            <span class="indent-{level + 1}">▼ Variant {idx + 1}: {variant_name}</span>
                        </td>
                    </tr>
'''

    def _format_occurs_row(self, name: str, schema: Dict, level: int, path: str,
                          offset: int, size: int, cobol_level: int, occurs_count: int) -> str:
        """Format HTML row for OCCURS array"""
        indent = f'indent-{level}'
        tree_icon = '├─' if level > 0 else ''

        return f'''                    <tr>
                        <td>{cobol_level:02d}</td>
                        <td class="field-name">
                            <span class="{indent}"><span class="tree-icon">{tree_icon}</span>{name}[{occurs_count}]</span>
                            <span class="badge badge-occurs">OCCURS {occurs_count}</span>
                        </td>
                        <td class="type-cell">ARRAY</td>
                        <td>{size}</td>
                        <td class="offset-cell">{offset}</td>
                        <td><button class="copy-btn" data-path="{path}">📋 Copy Path</button></td>
                    </tr>
'''

    def _format_elementary_row(self, name: str, schema: Dict, level: int,
                               path: str, offset: int) -> str:
        """Format HTML row for elementary item"""
        indent = f'indent-{level}'
        tree_icon = '├─' if level > 0 else ''

        # Get field type information
        pic_clause = schema.get('x-cobol-picture', 'N/A')
        field_type = schema.get('type', 'string')
        size = schema.get('x-cobol-size', 0)
        cobol_level = schema.get('x-cobol-level', 1)
        field_offset = schema.get('x-cobol-offset', offset)

        # Format type with USAGE if present
        usage = schema.get('x-cobol-usage', '')
        type_display = pic_clause
        if usage and usage != 'DISPLAY':
            type_display = f"{pic_clause} {usage}"

        return f'''                    <tr>
                        <td>{cobol_level:02d}</td>
                        <td class="field-name">
                            <span class="{indent}"><span class="tree-icon">{tree_icon}</span>{name}</span>
                        </td>
                        <td class="type-cell">{type_display}</td>
                        <td>{size}</td>
                        <td class="offset-cell">{field_offset}</td>
                        <td><button class="copy-btn" data-path="{path}">📋 Copy Path</button></td>
                    </tr>
'''

    def _javascript(self) -> str:
        """Generate JavaScript for copy and comparison functionality"""
        # Generate schema data for comparison
        schemas_json = json.dumps(self.schemas)

        return f'''        <script>
            // Schema data for comparison
            const schemas = {schemas_json};

            // Copy path to clipboard functionality
            document.querySelectorAll('.copy-btn').forEach(btn => {{
                btn.addEventListener('click', function() {{
                    const path = this.dataset.path;

                    // Copy to clipboard
                    if (navigator.clipboard) {{
                        navigator.clipboard.writeText(path).then(() => {{
                            // Visual feedback
                            this.textContent = '✓ Copied';
                            this.classList.add('copied');

                            setTimeout(() => {{
                                this.textContent = '📋 Copy Path';
                                this.classList.remove('copied');
                            }}, 1500);
                        }});
                    }} else {{
                        // Fallback for older browsers
                        const textArea = document.createElement('textarea');
                        textArea.value = path;
                        document.body.appendChild(textArea);
                        textArea.select();
                        document.execCommand('copy');
                        document.body.removeChild(textArea);

                        this.textContent = '✓ Copied';
                        this.classList.add('copied');
                        setTimeout(() => {{
                            this.textContent = '📋 Copy Path';
                            this.classList.remove('copied');
                        }}, 1500);
                    }}
                }});
            }});

            // Comparison functionality
            const compareBtn = document.getElementById('compare-btn');
            if (compareBtn) {{
                compareBtn.addEventListener('click', function() {{
                    const v1Index = parseInt(document.getElementById('variant1-select').value);
                    const v2Index = parseInt(document.getElementById('variant2-select').value);

                    if (v1Index === v2Index) {{
                        alert('Please select two different variants to compare');
                        return;
                    }}

                    const comparison = compareVariants(schemas[v1Index], schemas[v2Index]);
                    displayComparison(comparison, v1Index, v2Index);
                }});
            }}

            function flattenSchema(schema, parentPath = '') {{
                const fields = [];
                const properties = schema.properties || {{}};

                for (const [fieldName, fieldSchema] of Object.entries(properties)) {{
                    const fieldType = fieldSchema.type;
                    const fieldPath = parentPath ? `${{parentPath}}.${{fieldName}}` : fieldName;

                    const pic = fieldSchema['x-cobol-picture'] || '';
                    const usage = fieldSchema['x-cobol-usage'] || '';
                    let typeDisplay = pic || fieldType;
                    if (usage && usage !== 'DISPLAY') {{
                        typeDisplay = pic ? `${{pic}} ${{usage}}` : usage;
                    }}

                    const fieldInfo = {{
                        path: fieldPath,
                        type: typeDisplay,
                        size: fieldSchema['x-cobol-size'] || 0,
                        offset: fieldSchema['x-cobol-offset'] || 0
                    }};

                    if (fieldType === 'object' && !fieldSchema.oneOf) {{
                        fields.push(fieldInfo);
                        fields.push(...flattenSchema(fieldSchema, fieldPath));
                    }} else if (fieldType === 'array') {{
                        fields.push(fieldInfo);
                        const items = fieldSchema.items || {{}};
                        if (items.type === 'object') {{
                            fields.push(...flattenSchema(items, `${{fieldPath}}[0]`));
                        }}
                    }} else if (!fieldSchema.oneOf) {{
                        fields.push(fieldInfo);
                    }}
                }}

                return fields;
            }}

            function compareVariants(schema1, schema2) {{
                const fields1 = flattenSchema(schema1);
                const fields2 = flattenSchema(schema2);

                const map1 = Object.fromEntries(fields1.map(f => [f.path, f]));
                const map2 = Object.fromEntries(fields2.map(f => [f.path, f]));

                const paths1 = new Set(Object.keys(map1));
                const paths2 = new Set(Object.keys(map2));

                const result = {{
                    common: [],
                    onlyV1: [],
                    onlyV2: [],
                    typeDiff: []
                }};

                // Find common fields
                const commonPaths = [...paths1].filter(p => paths2.has(p));
                for (const path of commonPaths.sort()) {{
                    const f1 = map1[path];
                    const f2 = map2[path];

                    if (f1.type !== f2.type) {{
                        result.typeDiff.push({{ path, f1, f2 }});
                    }} else {{
                        result.common.push({{ path, field: f1 }});
                    }}
                }}

                // Fields only in variant 1
                const onlyV1 = [...paths1].filter(p => !paths2.has(p));
                result.onlyV1 = onlyV1.sort().map(path => ({{ path, field: map1[path] }}));

                // Fields only in variant 2
                const onlyV2 = [...paths2].filter(p => !paths1.has(p));
                result.onlyV2 = onlyV2.sort().map(path => ({{ path, field: map2[path] }}));

                return result;
            }}

            function displayComparison(comparison, v1Index, v2Index) {{
                const summaryDiv = document.getElementById('comparison-summary');
                const resultsDiv = document.getElementById('comparison-results');

                // Show summary
                summaryDiv.style.display = 'block';
                summaryDiv.innerHTML = `
                    <h3>Comparison Summary</h3>
                    <ul>
                        <li><strong>Common fields:</strong> ${{comparison.common.length}} (identical in both variants)</li>
                        <li><strong>Only in Variant ${{v1Index + 1}}:</strong> ${{comparison.onlyV1.length}} fields</li>
                        <li><strong>Only in Variant ${{v2Index + 1}}:</strong> ${{comparison.onlyV2.length}} fields</li>
                        <li><strong>Type differences:</strong> ${{comparison.typeDiff.length}} fields</li>
                    </ul>
                `;

                // Generate side-by-side comparison
                const rows = generateSideBySideRows(schemas[v1Index], schemas[v2Index]);

                const v1Name = getVariantName(schemas[v1Index], v1Index);
                const v2Name = getVariantName(schemas[v2Index], v2Index);

                let html = `
                    <div class="side-by-side-container">
                        <div class="variant-column">
                            <h3>Variant ${{v1Index + 1}}: ${{v1Name}}</h3>
                            <table class="comparison-table-side">
                                <thead>
                                    <tr>
                                        <th>Lvl</th>
                                        <th>Field Path</th>
                                        <th>Type</th>
                                        <th>Size</th>
                                        <th>Offset</th>
                                        <th>Action</th>
                                    </tr>
                                </thead>
                                <tbody id="variant1-tbody">
                `;

                for (const row of rows) {{
                    html += `<tr class="${{row.diffClass}}" data-row-id="${{row.rowId}}">${{row.v1Html}}</tr>`;
                }}

                html += `
                                </tbody>
                            </table>
                        </div>
                        <div class="variant-column">
                            <h3>Variant ${{v2Index + 1}}: ${{v2Name}}</h3>
                            <table class="comparison-table-side">
                                <thead>
                                    <tr>
                                        <th>Lvl</th>
                                        <th>Field Path</th>
                                        <th>Type</th>
                                        <th>Size</th>
                                        <th>Offset</th>
                                        <th>Action</th>
                                    </tr>
                                </thead>
                                <tbody id="variant2-tbody">
                `;

                for (const row of rows) {{
                    html += `<tr class="${{row.diffClass}}" data-row-id="${{row.rowId}}">${{row.v2Html}}</tr>`;
                }}

                html += `
                                </tbody>
                            </table>
                        </div>
                    </div>
                `;

                resultsDiv.innerHTML = html;

                // Re-attach copy button listeners
                attachCopyListeners();
            }}

            function getVariantName(schema, idx) {{
                const desc = schema.description || '';
                if (desc.toLowerCase().includes('variant:')) {{
                    const parts = desc.split('variant:');
                    if (parts.length > 1) {{
                        return parts[1].trim().split(' ')[0];
                    }}
                }}
                return `Variant ${{idx + 1}}`;
            }}

            function generateSideBySideRows(schema1, schema2, level = 0, parentPath = '', parentOffset = 0) {{
                const rows = [];
                const props1 = schema1.properties || {{}};
                const props2 = schema2.properties || {{}};

                // Get all unique field names
                const allFields = new Set([...Object.keys(props1), ...Object.keys(props2)]);
                const sortedFields = Array.from(allFields).sort();

                for (const fieldName of sortedFields) {{
                    const field1 = props1[fieldName];
                    const field2 = props2[fieldName];
                    const currentPath = parentPath ? `${{parentPath}}.${{fieldName}}` : fieldName;
                    const rowId = currentPath.replace(/\\./g, '-').replace(/\\[/g, '-').replace(/\\]/g, '');

                    // Determine diff class
                    let diffClass = '';
                    if (field1 && field2) {{
                        const type1 = getFieldTypeString(field1);
                        const type2 = getFieldTypeString(field2);
                        diffClass = (type1 === type2) ? 'diff-common' : 'diff-type-diff';
                    }} else if (field1) {{
                        diffClass = 'diff-v1-only';
                    }} else {{
                        diffClass = 'diff-v2-only';
                    }}

                    // Generate row HTML for both sides
                    const v1Html = renderFieldCell(field1, fieldName, level, currentPath, parentOffset, !!field1);
                    const v2Html = renderFieldCell(field2, fieldName, level, currentPath, parentOffset, !!field2);

                    rows.push({{ v1Html, v2Html, diffClass, rowId }});

                    // Recurse for groups
                    if (field1 && field2 && field1.type === 'object' && field2.type === 'object' && !field1.oneOf && !field2.oneOf) {{
                        const offset1 = (field1['x-cobol-offset'] || 0) + parentOffset;
                        const childRows = generateSideBySideRows(field1, field2, level + 1, currentPath, offset1);
                        rows.push(...childRows);
                    }}
                }}

                return rows;
            }}

            function getFieldTypeString(field) {{
                if (!field) return '';
                const type = field.type;
                if (type === 'object') return 'GROUP';
                if (type === 'array') return `ARRAY[${{field.maxItems || 1}}]`;
                const pic = field['x-cobol-picture'] || 'N/A';
                const usage = field['x-cobol-usage'] || '';
                return (usage && usage !== 'DISPLAY') ? `${{pic}} ${{usage}}` : pic;
            }}

            function renderFieldCell(field, fieldName, level, path, parentOffset, present) {{
                if (!present || !field) {{
                    const indent = level * 2;
                    const treeIcon = level > 0 ? '├─ ' : '';
                    return `
                        <td>--</td>
                        <td class="field-name">
                            <span style="margin-left: ${{indent}}em;"><span class="tree-icon">${{treeIcon}}</span>${{fieldName}}</span>
                        </td>
                        <td class="type-cell">[Not present]</td>
                        <td>--</td>
                        <td class="offset-cell">--</td>
                        <td>--</td>
                    `;
                }}

                const type = field.type;
                const offset = (field['x-cobol-offset'] || 0) + parentOffset;
                const size = field['x-cobol-size'] || 0;
                const cobolLevel = field['x-cobol-level'] || 1;
                const indent = level * 2;
                const treeIcon = level > 0 ? '├─ ' : '';

                let badge = '';
                let typeDisplay = '';
                let displayName = fieldName;

                if (type === 'object') {{
                    const oneOf = field.oneOf || [];
                    if (oneOf.length > 0) {{
                        badge = `<span class="badge badge-redefines">REDEFINES (${{oneOf.length}})</span>`;
                        typeDisplay = 'GROUP';
                    }} else {{
                        badge = '<span class="badge badge-group">GROUP</span>';
                        typeDisplay = 'GROUP';
                    }}
                }} else if (type === 'array') {{
                    const occurs = field.maxItems || 1;
                    badge = `<span class="badge badge-occurs">OCCURS ${{occurs}}</span>`;
                    typeDisplay = 'ARRAY';
                    displayName = `${{fieldName}}[${{occurs}}]`;
                }} else {{
                    const pic = field['x-cobol-picture'] || 'N/A';
                    const usage = field['x-cobol-usage'] || '';
                    typeDisplay = (usage && usage !== 'DISPLAY') ? `${{pic}} ${{usage}}` : pic;
                }}

                return `
                    <td>${{String(cobolLevel).padStart(2, '0')}}</td>
                    <td class="field-name">
                        <span style="margin-left: ${{indent}}em;"><span class="tree-icon">${{treeIcon}}</span>${{displayName}}</span>
                        ${{badge}}
                    </td>
                    <td class="type-cell">${{typeDisplay}}</td>
                    <td>${{size}}</td>
                    <td class="offset-cell">${{offset}}</td>
                    <td><button class="copy-btn" data-path="${{path}}">📋</button></td>
                `;
            }}

            function attachCopyListeners() {{
                document.querySelectorAll('.copy-btn').forEach(btn => {{
                    btn.addEventListener('click', function() {{
                        const path = this.dataset.path;
                        if (navigator.clipboard) {{
                            navigator.clipboard.writeText(path).then(() => {{
                                this.textContent = '✓';
                                this.classList.add('copied');
                                setTimeout(() => {{
                                    this.textContent = '📋';
                                    this.classList.remove('copied');
                                }}, 1500);
                            }});
                        }} else {{
                            const textArea = document.createElement('textarea');
                            textArea.value = path;
                            document.body.appendChild(textArea);
                            textArea.select();
                            document.execCommand('copy');
                            document.body.removeChild(textArea);
                            this.textContent = '✓';
                            this.classList.add('copied');
                            setTimeout(() => {{
                                this.textContent = '📋';
                                this.classList.remove('copied');
                            }}, 1500);
                        }}
                    }});
                }});
            }}
        </script>
'''

    def _generate_comparison_section(self) -> str:
        """Generate variant comparison section with side-by-side visualization"""
        html = []

        html.append('''
        <div id="comparison-section" class="comparison-section">
            <h2>Variant Comparison</h2>
            <div class="comparison-controls">
                <label for="variant1-select">Compare Variant:</label>
                <select id="variant1-select" class="variant-select">
''')

        # Add options for variant 1
        for idx in range(len(self.schemas)):
            variant_name = self._get_variant_name(self.schemas[idx], idx)
            html.append(f'                    <option value="{idx}">Variant {idx + 1}: {variant_name}</option>\n')

        html.append('''                </select>

                <label for="variant2-select">Against Variant:</label>
                <select id="variant2-select" class="variant-select">
''')

        # Add options for variant 2
        for idx in range(len(self.schemas)):
            variant_name = self._get_variant_name(self.schemas[idx], idx)
            selected = ' selected' if idx == 1 else ''
            html.append(f'                    <option value="{idx}"{selected}>Variant {idx + 1}: {variant_name}</option>\n')

        html.append('''                </select>

                <button id="compare-btn" class="compare-btn">Compare</button>
            </div>

            <div id="comparison-summary" class="comparison-summary" style="display:none;">
                <!-- Summary stats populated by JavaScript -->
            </div>

            <div id="comparison-results" class="comparison-results">
                <p class="comparison-placeholder">Select two variants and click "Compare" to see side-by-side comparison</p>
            </div>
        </div>
''')

        return ''.join(html)

    def _compare_variants(self, schema1: Dict, schema2: Dict) -> Dict[str, Any]:
        """
        Compare two variant schemas and return differences

        Args:
            schema1: First variant schema
            schema2: Second variant schema

        Returns:
            Dictionary with comparison results
        """
        result = {
            'common_fields': [],
            'only_in_v1': [],
            'only_in_v2': [],
            'type_differences': [],
            'size_differences': []
        }

        # Flatten both schemas to field lists
        fields1 = self._flatten_schema(schema1, '')
        fields2 = self._flatten_schema(schema2, '')

        # Create field maps by path
        map1 = {f['path']: f for f in fields1}
        map2 = {f['path']: f for f in fields2}

        # Find common, unique, and different fields
        paths1 = set(map1.keys())
        paths2 = set(map2.keys())

        # Common paths
        common_paths = paths1 & paths2
        for path in sorted(common_paths):
            f1 = map1[path]
            f2 = map2[path]

            # Check if types match
            if f1['type'] != f2['type']:
                result['type_differences'].append({
                    'path': path,
                    'v1_type': f1['type'],
                    'v2_type': f2['type'],
                    'v1_size': f1['size'],
                    'v2_size': f2['size']
                })
            elif f1['size'] != f2['size']:
                result['size_differences'].append({
                    'path': path,
                    'type': f1['type'],
                    'v1_size': f1['size'],
                    'v2_size': f2['size']
                })
            else:
                result['common_fields'].append({
                    'path': path,
                    'type': f1['type'],
                    'size': f1['size'],
                    'offset': f1['offset']
                })

        # Fields only in variant 1
        only_v1 = paths1 - paths2
        for path in sorted(only_v1):
            f = map1[path]
            result['only_in_v1'].append({
                'path': path,
                'type': f['type'],
                'size': f['size'],
                'offset': f['offset']
            })

        # Fields only in variant 2
        only_v2 = paths2 - paths1
        for path in sorted(only_v2):
            f = map2[path]
            result['only_in_v2'].append({
                'path': path,
                'type': f['type'],
                'size': f['size'],
                'offset': f['offset']
            })

        return result

    def _flatten_schema(self, schema: Dict, parent_path: str) -> List[Dict]:
        """
        Flatten schema to list of fields with paths

        Args:
            schema: Schema to flatten
            parent_path: Parent path prefix

        Returns:
            List of field dictionaries
        """
        fields = []
        properties = schema.get('properties', {})

        for field_name, field_schema in properties.items():
            field_type = field_schema.get('type')
            field_path = f"{parent_path}.{field_name}" if parent_path else field_name

            # Get field metadata
            pic = field_schema.get('x-cobol-picture', '')
            usage = field_schema.get('x-cobol-usage', '')
            type_display = pic if pic else field_type
            if usage and usage != 'DISPLAY':
                type_display = f"{pic} {usage}" if pic else usage

            field_info = {
                'path': field_path,
                'type': type_display,
                'size': field_schema.get('x-cobol-size', 0),
                'offset': field_schema.get('x-cobol-offset', 0)
            }

            if field_type == 'object' and 'oneOf' not in field_schema:
                # Regular group - add and recurse
                fields.append(field_info)
                fields.extend(self._flatten_schema(field_schema, field_path))
            elif field_type == 'array':
                # OCCURS array - add array itself
                fields.append(field_info)
                # Add [0] element structure
                items = field_schema.get('items', {})
                if items.get('type') == 'object':
                    fields.extend(self._flatten_schema(items, f"{field_path}[0]"))
            elif 'oneOf' in field_schema:
                # REDEFINES - skip, handled separately
                # We only compare within same variant, not across REDEFINES
                pass
            else:
                # Elementary field
                fields.append(field_info)

        return fields

    def _generate_side_by_side_rows(self, schema1: Dict, schema2: Dict, level: int = 0,
                                     parent_path: str = '', parent_offset: int = 0) -> List[tuple]:
        """
        Generate aligned rows for side-by-side comparison

        Returns list of tuples: (v1_row_html, v2_row_html, diff_class, row_id)
        """
        rows = []

        # Get properties from both schemas
        props1 = schema1.get('properties', {})
        props2 = schema2.get('properties', {})

        # Get all unique field names from both variants
        all_fields = sorted(set(props1.keys()) | set(props2.keys()))

        for field_name in all_fields:
            field1 = props1.get(field_name)
            field2 = props2.get(field_name)

            current_path = f"{parent_path}.{field_name}" if parent_path else field_name
            row_id = f"row-{current_path.replace('.', '-').replace('[', '-').replace(']', '')}"

            # Determine difference class
            if field1 and field2:
                # Both have the field - check if types match
                type1 = self._get_field_type_string(field1)
                type2 = self._get_field_type_string(field2)
                if type1 == type2:
                    diff_class = 'diff-common'
                else:
                    diff_class = 'diff-type-diff'
            elif field1:
                diff_class = 'diff-v1-only'
            else:
                diff_class = 'diff-v2-only'

            # Generate HTML for both sides
            v1_html = self._render_field_row(field1, field_name, level, current_path,
                                              parent_offset, present=bool(field1))
            v2_html = self._render_field_row(field2, field_name, level, current_path,
                                              parent_offset, present=bool(field2))

            rows.append((v1_html, v2_html, diff_class, row_id))

            # Process children if both are groups
            if field1 and field2:
                type1 = field1.get('type')
                type2 = field2.get('type')
                if type1 == 'object' and type2 == 'object':
                    # Both are groups - recurse
                    offset1 = field1.get('x-cobol-offset', 0) + parent_offset
                    child_rows = self._generate_side_by_side_rows(
                        field1, field2, level + 1, current_path, offset1
                    )
                    rows.extend(child_rows)

        return rows

    def _get_field_type_string(self, field_schema: Dict) -> str:
        """Get a string representation of field type for comparison"""
        if not field_schema:
            return ""

        field_type = field_schema.get('type')
        if field_type == 'object':
            return "GROUP"
        elif field_type == 'array':
            return f"ARRAY[{field_schema.get('maxItems', 1)}]"
        else:
            pic = field_schema.get('x-cobol-picture', 'N/A')
            usage = field_schema.get('x-cobol-usage', '')
            if usage and usage != 'DISPLAY':
                return f"{pic} {usage}"
            return pic

    def _render_field_row(self, field_schema: Dict, field_name: str, level: int,
                          path: str, parent_offset: int, present: bool = True) -> str:
        """Render a single field row for side-by-side display"""
        if not present or not field_schema:
            # Field not present in this variant
            indent = f'indent-{level}'
            tree_icon = '├─' if level > 0 else ''
            return f'''<tr class="field-absent">
                <td>--</td>
                <td class="field-name">
                    <span class="{indent}"><span class="tree-icon">{tree_icon}</span>{field_name}</span>
                </td>
                <td class="type-cell">[Not present]</td>
                <td>--</td>
                <td class="offset-cell">--</td>
                <td>--</td>
            </tr>'''

        # Field is present
        field_type = field_schema.get('type')
        field_offset = field_schema.get('x-cobol-offset', 0) + parent_offset
        field_size = field_schema.get('x-cobol-size', 0)
        cobol_level = field_schema.get('x-cobol-level', 1)

        indent = f'indent-{level}'
        tree_icon = '├─' if level > 0 else ''

        # Determine field type display
        if field_type == 'object':
            oneOf = field_schema.get('oneOf', [])
            if oneOf:
                variant_count = len(oneOf)
                badge = f'<span class="badge badge-redefines">REDEFINES ({variant_count})</span>'
                type_display = 'GROUP'
            else:
                badge = '<span class="badge badge-group">GROUP</span>'
                type_display = 'GROUP'
        elif field_type == 'array':
            occurs_count = field_schema.get('maxItems', 1)
            badge = f'<span class="badge badge-occurs">OCCURS {occurs_count}</span>'
            type_display = 'ARRAY'
            field_name = f"{field_name}[{occurs_count}]"
        else:
            badge = ''
            pic_clause = field_schema.get('x-cobol-picture', 'N/A')
            usage = field_schema.get('x-cobol-usage', '')
            if usage and usage != 'DISPLAY':
                type_display = f"{pic_clause} {usage}"
            else:
                type_display = pic_clause

        copy_btn = f'<button class="copy-btn" data-path="{path}">📋</button>'

        return f'''<tr class="field-present">
                <td>{cobol_level:02d}</td>
                <td class="field-name">
                    <span class="{indent}"><span class="tree-icon">{tree_icon}</span>{field_name}</span>
                    {badge}
                </td>
                <td class="type-cell">{type_display}</td>
                <td>{field_size}</td>
                <td class="offset-cell">{field_offset}</td>
                <td>{copy_btn}</td>
            </tr>'''

    def _html_footer(self) -> str:
        """Generate HTML footer"""
        return '''        <footer>
            <p>Generated by COBOL Copybook HTML Visualizer</p>
            <p>Use "Copy Path" buttons to copy field paths for code generation</p>
        </footer>
    </div>
</body>
</html>
'''


def main():
    """Command-line interface"""
    parser = argparse.ArgumentParser(
        description='Generate HTML visualization from COBOL copybook JSON schema'
    )
    parser.add_argument(
        'schema_file',
        help='Path to JSON schema file'
    )
    parser.add_argument(
        '-o', '--output',
        default='copybook_visualization.html',
        help='Output HTML file (default: copybook_visualization.html)'
    )
    parser.add_argument(
        '--open',
        action='store_true',
        help='Open generated HTML in browser'
    )
    parser.add_argument(
        '--compare',
        action='store_true',
        help='Enable variant comparison feature (for multi-variant schemas)'
    )

    args = parser.parse_args()

    # Validate input file
    if not Path(args.schema_file).exists():
        print(f"Error: Schema file not found: {args.schema_file}")
        sys.exit(1)

    # Generate HTML
    print(f"Generating HTML visualization from {args.schema_file}...")
    generator = CopybookHTMLGenerator(args.schema_file, enable_comparison=args.compare)
    html_content = generator.generate()

    # Write output file
    output_path = Path(args.output)
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html_content)

    print(f"✓ Generated: {output_path}")
    print(f"  File size: {len(html_content):,} bytes")
    print(f"  Variants: {len(generator.schemas)}")

    # Open in browser if requested
    if args.open:
        print(f"Opening {output_path} in browser...")
        webbrowser.open(f'file://{output_path.absolute()}')

    print("\nUsage: Open the HTML file in any web browser")
    print("       Click 'Copy Path' buttons to copy field paths for code generation")


if __name__ == '__main__':
    main()
