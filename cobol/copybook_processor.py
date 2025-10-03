"""
COBOL Copybook AST Processor
Performs semantic analysis on AST: computes offsets, sizes, and handles REDEFINES.
"""

import sys
from typing import List, Dict, Set, Tuple, Optional, Generator
from dataclasses import dataclass, field
from copy import deepcopy
from copybook_parser import ASTNode, NodeType, PictureClause, UsageClause

# Increase int conversion limit for large copybooks
# Python 3.11+ has a default limit of 4300 digits
sys.set_int_max_str_digits(0)  # 0 = unlimited


@dataclass
class RedefinesGroup:
    """Represents a group of fields that redefine the same memory location"""
    original_field: str
    variants: List[ASTNode] = field(default_factory=list)
    offset: int = 0
    max_size: int = 0


@dataclass
class ProcessedCopybook:
    """Result of processing a copybook"""
    root: ASTNode
    total_size: int
    redefines_groups: List[RedefinesGroup]
    field_map: Dict[str, ASTNode]


class CopybookProcessor:
    """Processes AST to compute offsets, sizes, and handle REDEFINES"""

    def __init__(self, ast: ASTNode):
        """
        Initialize processor

        Args:
            ast: Root AST node from parser
        """
        self.ast = ast
        self.field_map: Dict[str, ASTNode] = {}
        self.redefines_groups: List[RedefinesGroup] = []
        self.current_path: List[str] = []

    def process(self) -> ProcessedCopybook:
        """
        Process the AST

        Returns:
            ProcessedCopybook with computed offsets and sizes
        """
        # Build field map and identify REDEFINES groups
        self._build_field_map(self.ast)

        # Identify REDEFINES groups
        self._identify_redefines_groups(self.ast)

        # Compute offsets and sizes
        total_size = self._compute_offsets_and_sizes(self.ast, 0)

        return ProcessedCopybook(
            root=self.ast,
            total_size=total_size,
            redefines_groups=self.redefines_groups,
            field_map=self.field_map
        )

    def _build_field_map(self, node: ASTNode, parent_path: str = ''):
        """Build a map of all fields with their full paths"""
        # Create full path
        if parent_path:
            full_path = f"{parent_path}.{node.name}"
        else:
            full_path = node.name

        self.field_map[full_path] = node

        # Recursively process children
        for child in node.children:
            self._build_field_map(child, full_path)

    def _identify_redefines_groups(self, node: ASTNode):
        """Identify groups of fields that redefine the same location"""
        # Group children by what they redefine
        redefines_map: Dict[str, List[ASTNode]] = {}
        original_fields: Dict[str, ASTNode] = {}

        for child in node.children:
            if child.redefines:
                if child.redefines not in redefines_map:
                    redefines_map[child.redefines] = []
                redefines_map[child.redefines].append(child)
            else:
                original_fields[child.name] = child

            # Recursively process children
            self._identify_redefines_groups(child)

        # Create RedefinesGroup objects
        for original_name, redefining_fields in redefines_map.items():
            if original_name in original_fields:
                group = RedefinesGroup(
                    original_field=original_name,
                    variants=[original_fields[original_name]] + redefining_fields
                )
                self.redefines_groups.append(group)

    def _compute_offsets_and_sizes(self, node: ASTNode, current_offset: int) -> int:
        """
        Compute byte offsets and storage sizes for all fields

        Args:
            node: Current AST node
            current_offset: Current byte offset

        Returns:
            Total size of this node and its children
        """
        node.offset = current_offset

        if node.is_elementary():
            # Elementary item - calculate storage size
            size = self._calculate_storage_size(node)
            node.storage_size = size

            # Handle OCCURS clause
            if node.occurs:
                size *= node.occurs

            return size

        elif node.is_group():
            # Group item - sum of children sizes
            total_size = 0
            child_offset = current_offset

            # Track which fields redefine others
            redefines_tracker: Dict[str, int] = {}

            for child in node.children:
                if child.redefines:
                    # Redefining field - use offset of original field
                    if child.redefines in redefines_tracker:
                        original_offset = redefines_tracker[child.redefines]
                        child_size = self._compute_offsets_and_sizes(child, original_offset)

                        # Update max size for this redefined location
                        existing_size = redefines_tracker.get(f"_size_{child.redefines}", 0)
                        redefines_tracker[f"_size_{child.redefines}"] = max(existing_size, child_size)
                    else:
                        # First REDEFINES - find the original field
                        for prev_child in node.children:
                            if prev_child.name == child.redefines:
                                redefines_tracker[child.redefines] = prev_child.offset
                                child_size = self._compute_offsets_and_sizes(child, prev_child.offset)
                                redefines_tracker[f"_size_{child.redefines}"] = max(
                                    prev_child.storage_size, child_size
                                )
                                break
                else:
                    # Normal field - sequential offset
                    child_size = self._compute_offsets_and_sizes(child, child_offset)
                    redefines_tracker[child.name] = child_offset
                    redefines_tracker[f"_size_{child.name}"] = child_size

                    child_offset += child_size
                    total_size = child_offset - current_offset

            node.storage_size = total_size

            # Handle OCCURS clause for groups
            if node.occurs:
                total_size *= node.occurs

            return total_size

        return 0

    def _calculate_storage_size(self, node: ASTNode) -> int:
        """Calculate storage size in bytes for an elementary item"""
        if not node.picture:
            return 0

        display_length = node.picture.display_length

        # Apply USAGE clause
        if node.usage:
            multiplier = node.usage.get_storage_multiplier(display_length)
            size = int(display_length * multiplier)
        else:
            size = display_length

        # Adjust for SIGN SEPARATE
        if node.sign_separate:
            size += 1

        return max(size, 1)  # Minimum 1 byte

    def get_redefines_combinations(self) -> List[List[Tuple[str, ASTNode]]]:
        """
        Generate all combinations of REDEFINES choices

        Returns:
            List of combinations, where each combination is a list of (path, node) tuples
        """
        if not self.redefines_groups:
            return [[]]

        # Generate Cartesian product of all REDEFINES groups
        combinations = [[]]

        for group in self.redefines_groups:
            new_combinations = []
            for combo in combinations:
                for variant in group.variants:
                    new_combo = combo + [(group.original_field, variant)]
                    new_combinations.append(new_combo)
            combinations = new_combinations

        return combinations

    def create_variant_tree(self, combination: List[Tuple[str, ASTNode]]) -> ASTNode:
        """
        Create a variant AST tree with specific REDEFINES choices

        Args:
            combination: List of (original_field_name, chosen_variant) tuples

        Returns:
            New AST tree with REDEFINES resolved
        """
        # Deep copy the original tree
        variant_tree = deepcopy(self.ast)

        # Build a mapping of which variants to keep
        redefines_choices = {original: variant.name for original, variant in combination}

        # Remove unchosen REDEFINES variants
        self._prune_redefines(variant_tree, redefines_choices)

        return variant_tree

    def _prune_redefines(self, node: ASTNode, choices: Dict[str, str]):
        """
        Remove REDEFINES variants that weren't chosen

        Args:
            node: Current node
            choices: Map of original_field -> chosen_variant_name
        """
        # Filter children
        new_children = []
        redefines_seen: Set[str] = set()

        for child in node.children:
            if child.redefines:
                # This is a REDEFINES field
                if child.redefines in choices:
                    # Check if this variant was chosen
                    if choices[child.redefines] == child.name:
                        # Keep this variant, but clear the REDEFINES marker
                        child.redefines = None
                        new_children.append(child)
                    # Otherwise, skip this variant
                else:
                    # No choice specified, keep first variant
                    if child.redefines not in redefines_seen:
                        redefines_seen.add(child.redefines)
                        child.redefines = None
                        new_children.append(child)
            else:
                # Normal field, always keep
                new_children.append(child)

        node.children = new_children

        # Recursively process children
        for child in node.children:
            self._prune_redefines(child, choices)


class RedefinesEnumerator:
    """Enumerates all REDEFINES combinations and organizes them into groups"""

    def __init__(self, processed: ProcessedCopybook):
        """
        Initialize enumerator

        Args:
            processed: ProcessedCopybook from CopybookProcessor
        """
        self.processed = processed
        # Create processor and copy over the redefines groups
        self.processor = CopybookProcessor(processed.root)
        self.processor.redefines_groups = processed.redefines_groups

    def enumerate_variants(self) -> List[Tuple[str, ASTNode]]:
        """
        Enumerate all REDEFINES variants

        Returns:
            List of (variant_name, variant_tree) tuples
        """
        combinations = self.processor.get_redefines_combinations()

        variants = []
        for i, combo in enumerate(combinations, start=1):
            # Generate variant name
            if combo:
                variant_name = "_".join([variant.name for _, variant in combo])
            else:
                variant_name = "default"

            # Create variant tree
            variant_tree = self.processor.create_variant_tree(combo)

            variants.append((variant_name, variant_tree))

        return variants

    def group_variants_by_top_level(self, variants: List[Tuple[str, ASTNode]]) -> Dict[str, List[Tuple[str, ASTNode]]]:
        """
        Group variants by their top-level REDEFINES choice

        This allows generating separate files for each top-level choice
        as requested by the user (e.g., 2 files for 2 top-level variants)

        Args:
            variants: List of (variant_name, variant_tree) tuples

        Returns:
            Dictionary mapping top-level variant name to list of variants
        """
        if not self.processed.redefines_groups:
            return {"default": variants}

        groups: Dict[str, List[Tuple[str, ASTNode]]] = {}

        for variant_name, variant_tree in variants:
            # Extract top-level REDEFINES choice from variant name
            parts = variant_name.split('_')
            if parts:
                top_level = parts[0]
            else:
                top_level = "default"

            if top_level not in groups:
                groups[top_level] = []

            groups[top_level].append((variant_name, variant_tree))

        return groups


class HierarchicalRedefinesEnumerator:
    """
    Hierarchical REDEFINES enumeration for large copybooks.
    Only enumerates variants within active hierarchy paths.
    """

    def __init__(self, processed: ProcessedCopybook, max_variants: int = 10000, max_depth: Optional[int] = None):
        """
        Initialize hierarchical enumerator

        Args:
            processed: ProcessedCopybook from CopybookProcessor
            max_variants: Maximum number of variants to generate (safety limit)
            max_depth: Maximum depth of REDEFINES to enumerate (None = unlimited)
        """
        self.processed = processed
        self.processor = CopybookProcessor(processed.root)
        self.processor.redefines_groups = processed.redefines_groups
        self.max_variants = max_variants
        self.max_depth = max_depth
        self.variant_count = 0

    def enumerate_variants_streaming(self) -> Generator[Tuple[str, ASTNode], None, None]:
        """
        Stream variant generation to avoid memory explosion.
        Yields variants one at a time.

        Yields:
            (variant_name, variant_tree) tuples
        """
        self.variant_count = 0

        # Group REDEFINES by their parent path for hierarchical processing
        hierarchy_map = self._build_hierarchy_map()

        # Start enumeration from root
        yield from self._enumerate_hierarchical(
            node=deepcopy(self.processed.root),
            hierarchy_map=hierarchy_map,
            current_path="",
            current_depth=0,
            variant_prefix=""
        )

    def _build_hierarchy_map(self) -> Dict[str, List[RedefinesGroup]]:
        """
        Build a map of REDEFINES groups organized by their parent path.

        Returns:
            Dictionary mapping parent paths to REDEFINES groups at that level
        """
        hierarchy_map: Dict[str, List[RedefinesGroup]] = {}

        def traverse(node: ASTNode, path: str):
            # Check if this node has REDEFINES children
            redefines_at_this_level = []

            for child in node.children:
                if child.redefines:
                    # Find the group this belongs to
                    for group in self.processed.redefines_groups:
                        if group.original_field == child.redefines:
                            if group not in redefines_at_this_level:
                                redefines_at_this_level.append(group)
                            break

            if redefines_at_this_level:
                hierarchy_map[path] = redefines_at_this_level

            # Recurse into children
            for child in node.children:
                if not child.redefines:  # Don't traverse REDEFINES nodes yet
                    child_path = f"{path}.{child.name}" if path else child.name
                    traverse(child, child_path)

        traverse(self.processed.root, "")
        return hierarchy_map

    def _enumerate_hierarchical(
        self,
        node: ASTNode,
        hierarchy_map: Dict[str, List[RedefinesGroup]],
        current_path: str,
        current_depth: int,
        variant_prefix: str
    ) -> Generator[Tuple[str, ASTNode], None, None]:
        """
        Recursively enumerate variants in hierarchical context.

        Args:
            node: Current node being processed
            hierarchy_map: Map of parent paths to REDEFINES groups
            current_path: Current path in the tree
            current_depth: Current REDEFINES nesting depth
            variant_prefix: Accumulated variant name prefix
        """
        # Check depth limit
        if self.max_depth is not None and current_depth >= self.max_depth:
            # Stop enumeration, yield this variant
            variant_name = variant_prefix if variant_prefix else "default"
            self.variant_count += 1
            yield (variant_name, node)
            return

        # Check if there are REDEFINES at this level
        if current_path in hierarchy_map:
            # Enumerate each variant at this level
            for group in hierarchy_map[current_path]:
                for variant in group.variants:
                    # Safety check
                    if self.variant_count >= self.max_variants:
                        print(f"\nWarning: Reached maximum variant limit ({self.max_variants})")
                        print(f"Use --max-variants to increase or --max-depth to limit enumeration depth")
                        return

                    # Create a copy with this variant choice
                    variant_node = deepcopy(node)

                    # Apply this REDEFINES choice
                    self._apply_redefines_choice(variant_node, group.original_field, variant.name)

                    # Build variant name
                    new_prefix = f"{variant_prefix}_{variant.name}" if variant_prefix else variant.name

                    # Recurse into this variant's children
                    variant_path = f"{current_path}.{variant.name}" if current_path else variant.name

                    # Check if there are nested REDEFINES
                    has_nested = any(p.startswith(variant_path + ".") for p in hierarchy_map.keys())

                    if has_nested:
                        # Continue enumeration in nested context
                        yield from self._enumerate_hierarchical(
                            node=variant_node,
                            hierarchy_map=hierarchy_map,
                            current_path=variant_path,
                            current_depth=current_depth + 1,
                            variant_prefix=new_prefix
                        )
                    else:
                        # No nested REDEFINES, yield this variant
                        self.variant_count += 1
                        yield (new_prefix, variant_node)
        else:
            # No REDEFINES at this level, yield current state
            variant_name = variant_prefix if variant_prefix else "default"
            self.variant_count += 1
            yield (variant_name, node)

    def _apply_redefines_choice(self, node: ASTNode, original_field: str, variant_name: str):
        """
        Apply a REDEFINES choice to a node tree (in-place modification).

        Args:
            node: Node to modify
            original_field: Original field being redefined
            variant_name: Name of variant to keep
        """
        # Filter children
        new_children = []
        redefines_seen: Set[str] = set()

        for child in node.children:
            if child.redefines:
                # This is a REDEFINES field
                if child.redefines == original_field:
                    # Check if this is the chosen variant
                    if child.name == variant_name:
                        # Keep this variant, clear REDEFINES marker
                        child.redefines = None
                        new_children.append(child)
                        redefines_seen.add(original_field)
                    # Otherwise skip this variant
                elif child.redefines not in redefines_seen:
                    # Different REDEFINES group, keep first variant as default
                    child.redefines = None
                    new_children.append(child)
                    redefines_seen.add(child.redefines if child.redefines else child.name)
            else:
                # Normal field, always keep
                new_children.append(child)

        node.children = new_children

        # Recursively process children
        for child in node.children:
            self._apply_redefines_choice(child, original_field, variant_name)


if __name__ == '__main__':
    import sys
    from copybook_parser import parse_copybook

    if len(sys.argv) > 1:
        # Parse copybook
        ast = parse_copybook(sys.argv[1])

        # Process AST
        processor = CopybookProcessor(ast)
        processed = processor.process()

        print(f"Total size: {processed.total_size} bytes")
        print(f"REDEFINES groups: {len(processed.redefines_groups)}")

        for group in processed.redefines_groups:
            print(f"\nREDEFINES group for '{group.original_field}':")
            for variant in group.variants:
                print(f"  - {variant.name}")

        # Enumerate variants
        enumerator = RedefinesEnumerator(processed)
        variants = enumerator.enumerate_variants()

        print(f"\nTotal variants: {len(variants)}")
        for name, _ in variants[:10]:  # Show first 10
            print(f"  - {name}")

    else:
        print("Usage: python copybook_processor.py <copybook_file>")
