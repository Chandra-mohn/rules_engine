"""
COBOL Copybook Parser
Builds an Abstract Syntax Tree (AST) from tokenized copybook.
"""

from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any
from enum import Enum, auto
from copybook_lexer import Token, TokenType, CobolLexer


class NodeType(Enum):
    """AST Node types"""
    COPYBOOK = auto()
    GROUP = auto()
    ELEMENTARY = auto()
    REDEFINES = auto()


@dataclass
class PictureClause:
    """Represents a PICTURE/PIC clause"""
    picture_string: str
    display_length: int = 0
    storage_length: int = 0
    data_type: str = 'alphanumeric'  # alphanumeric, numeric, numeric-edited

    def __post_init__(self):
        """Calculate lengths and data type from picture string"""
        self._parse_picture()

    def _parse_picture(self):
        """Parse PICTURE string to determine type and length"""
        pic = self.picture_string.upper()

        # Numeric picture
        if '9' in pic or 'S' in pic:
            self.data_type = 'numeric'
            self.display_length = self._calculate_length(pic)
            self.storage_length = self.display_length

        # Alphanumeric picture
        elif 'X' in pic:
            self.data_type = 'alphanumeric'
            self.display_length = self._calculate_length(pic)
            self.storage_length = self.display_length

        # Alphabetic picture
        elif 'A' in pic:
            self.data_type = 'alphabetic'
            self.display_length = self._calculate_length(pic)
            self.storage_length = self.display_length

    def _calculate_length(self, pic: str) -> int:
        """Calculate display length from PICTURE string"""
        length = 0
        i = 0

        while i < len(pic):
            char = pic[i]

            # Handle repetition: X(10) or 9(5)
            if i + 1 < len(pic) and pic[i + 1] == '(':
                # Find closing parenthesis
                j = i + 2
                while j < len(pic) and pic[j] != ')':
                    j += 1
                if j < len(pic):
                    count_str = pic[i + 2:j]
                    try:
                        count = int(count_str)
                        length += count
                    except ValueError:
                        length += 1
                    i = j + 1
                    continue

            # Single character
            if char in ('9', 'X', 'A', 'Z', '*', '+', '-', ',', '.', '/'):
                length += 1

            i += 1

        return length


@dataclass
class UsageClause:
    """Represents a USAGE/COMP clause"""
    usage_type: str = 'DISPLAY'  # DISPLAY, COMP, COMP-1, COMP-2, COMP-3, COMP-4, etc.

    def get_storage_multiplier(self, display_length: int) -> float:
        """Calculate storage bytes based on usage type"""
        if display_length == 0:
            return 1.0

        if self.usage_type == 'DISPLAY':
            return 1.0
        elif self.usage_type in ('COMP', 'COMP-4', 'BINARY'):
            # Binary: 1-4 digits = 2 bytes, 5-9 = 4 bytes, 10-18 = 8 bytes
            if display_length <= 4:
                return 2 / display_length
            elif display_length <= 9:
                return 4 / display_length
            else:
                return 8 / display_length
        elif self.usage_type in ('COMP-3', 'PACKED-DECIMAL'):
            # Packed decimal: (digits + 1) / 2
            return (display_length + 1) / (2 * display_length)
        elif self.usage_type == 'COMP-1':
            return 4 / display_length  # 4 bytes for single precision float
        elif self.usage_type == 'COMP-2':
            return 8 / display_length  # 8 bytes for double precision float
        return 1.0


@dataclass
class ASTNode:
    """Base class for AST nodes"""
    node_type: NodeType
    level: int
    name: str
    line_number: int
    children: List['ASTNode'] = field(default_factory=list)

    # Optional clauses
    picture: Optional[PictureClause] = None
    usage: Optional[UsageClause] = None
    occurs: Optional[int] = None
    redefines: Optional[str] = None
    sign_clause: Optional[str] = None  # LEADING/TRAILING
    sign_separate: bool = False
    justified: bool = False
    justified_right: bool = False
    blank_when_zero: bool = False
    value: Optional[str] = None

    # Computed fields
    offset: int = 0
    storage_size: int = 0

    def is_group(self) -> bool:
        """Check if this is a group item"""
        return self.node_type == NodeType.GROUP

    def is_elementary(self) -> bool:
        """Check if this is an elementary item"""
        return self.node_type == NodeType.ELEMENTARY

    def is_redefines(self) -> bool:
        """Check if this redefines another field"""
        return self.redefines is not None

    def add_child(self, child: 'ASTNode'):
        """Add a child node"""
        self.children.append(child)

    def get_full_name(self, separator: str = '.') -> str:
        """Get fully qualified name (for path generation)"""
        # This will be populated during tree traversal
        return self.name


class CobolParser:
    """Recursive descent parser for COBOL copybooks"""

    def __init__(self, tokens: List[Token]):
        """
        Initialize parser

        Args:
            tokens: List of tokens from lexer
        """
        self.tokens = tokens
        self.current = 0
        self.root = None

    def parse(self) -> ASTNode:
        """Parse tokens into AST"""
        # Find the first level 01 item
        while not self._is_at_end() and not self._check(TokenType.LEVEL_NUMBER):
            self._advance()

        if self._is_at_end():
            raise Exception("No level 01 record found")

        # Parse the root level 01
        self.root = self._parse_item()

        return self.root

    def _parse_item(self) -> ASTNode:
        """Parse a single data item (group or elementary)"""
        if not self._check(TokenType.LEVEL_NUMBER):
            raise Exception(f"Expected level number at line {self._peek().line}")

        level_token = self._advance()
        level = int(level_token.value)

        # Name
        if not self._check(TokenType.IDENTIFIER):
            raise Exception(f"Expected identifier at line {self._peek().line}")

        name_token = self._advance()
        name = name_token.value

        # Create node (will determine type after parsing clauses)
        node = ASTNode(
            node_type=NodeType.GROUP,  # Default, will update
            level=level,
            name=name,
            line_number=name_token.line
        )

        # Parse clauses
        has_picture = False
        while not self._is_at_end() and not self._check(TokenType.PERIOD):
            if self._check(TokenType.REDEFINES):
                self._advance()
                if self._check(TokenType.IDENTIFIER):
                    node.redefines = self._advance().value
                    node.node_type = NodeType.REDEFINES

            elif self._check_any(TokenType.PIC, TokenType.PICTURE):
                self._advance()
                pic_string = self._parse_picture_string()
                node.picture = PictureClause(pic_string)
                has_picture = True
                node.node_type = NodeType.ELEMENTARY

            elif self._check(TokenType.OCCURS):
                self._advance()
                if self._check(TokenType.NUMERIC_LITERAL):
                    node.occurs = int(self._advance().value)
                # Skip TIMES keyword if present
                if self._check(TokenType.TIMES):
                    self._advance()

            elif self._check_any(TokenType.COMP, TokenType.COMP_1, TokenType.COMP_2,
                                 TokenType.COMP_3, TokenType.COMP_4, TokenType.COMPUTATIONAL,
                                 TokenType.COMPUTATIONAL_1, TokenType.COMPUTATIONAL_2,
                                 TokenType.COMPUTATIONAL_3, TokenType.COMPUTATIONAL_4):
                usage_token = self._advance()
                usage_map = {
                    TokenType.COMP: 'COMP',
                    TokenType.COMP_1: 'COMP-1',
                    TokenType.COMP_2: 'COMP-2',
                    TokenType.COMP_3: 'COMP-3',
                    TokenType.COMP_4: 'COMP-4',
                    TokenType.COMPUTATIONAL: 'COMP',
                    TokenType.COMPUTATIONAL_1: 'COMP-1',
                    TokenType.COMPUTATIONAL_2: 'COMP-2',
                    TokenType.COMPUTATIONAL_3: 'COMP-3',
                    TokenType.COMPUTATIONAL_4: 'COMP-4',
                }
                node.usage = UsageClause(usage_map.get(usage_token.type, 'DISPLAY'))

            elif self._check(TokenType.USAGE):
                self._advance()
                if self._check(TokenType.IS):
                    self._advance()
                if self._check_any(TokenType.COMP, TokenType.BINARY, TokenType.PACKED_DECIMAL,
                                  TokenType.DISPLAY):
                    usage_token = self._advance()
                    usage_map = {
                        TokenType.COMP: 'COMP',
                        TokenType.BINARY: 'COMP',
                        TokenType.PACKED_DECIMAL: 'COMP-3',
                        TokenType.DISPLAY: 'DISPLAY',
                    }
                    node.usage = UsageClause(usage_map.get(usage_token.type, 'DISPLAY'))

            elif self._check(TokenType.SIGN):
                self._advance()
                if self._check(TokenType.IS):
                    self._advance()
                if self._check(TokenType.LEADING):
                    node.sign_clause = 'LEADING'
                    self._advance()
                elif self._check(TokenType.TRAILING):
                    node.sign_clause = 'TRAILING'
                    self._advance()
                if self._check(TokenType.SEPARATE):
                    self._advance()
                    node.sign_separate = True
                    if self._check(TokenType.CHARACTER):
                        self._advance()

            elif self._check_any(TokenType.JUSTIFIED, TokenType.JUST):
                self._advance()
                node.justified = True
                if self._check(TokenType.RIGHT):
                    self._advance()
                    node.justified_right = True

            elif self._check(TokenType.BLANK):
                self._advance()
                if self._check(TokenType.WHEN):
                    self._advance()
                if self._check(TokenType.ZERO):
                    self._advance()
                    node.blank_when_zero = True

            elif self._check(TokenType.VALUE):
                self._advance()
                if self._check(TokenType.IS):
                    self._advance()
                if self._check_any(TokenType.STRING_LITERAL, TokenType.NUMERIC_LITERAL):
                    node.value = self._advance().value

            else:
                # Unknown token, skip it
                self._advance()

        # Consume period
        if self._check(TokenType.PERIOD):
            self._advance()

        # If no PICTURE clause and not explicitly REDEFINES, it's a group
        if not has_picture and node.node_type != NodeType.REDEFINES:
            node.node_type = NodeType.GROUP

        # Parse children (items with higher level numbers)
        while not self._is_at_end():
            # Peek at next level number
            if self._check(TokenType.LEVEL_NUMBER):
                next_level = int(self._peek().value)
                if next_level > level:
                    child = self._parse_item()
                    node.add_child(child)
                else:
                    # Same or lower level, stop parsing children
                    break
            else:
                self._advance()

        return node

    def _parse_picture_string(self) -> str:
        """Parse a PICTURE string (handles various formats)"""
        pic_parts = []

        # Picture string can be an identifier (like X(10)) or series of tokens
        while not self._is_at_end() and not self._check_any(TokenType.PERIOD, TokenType.OCCURS,
                                                             TokenType.COMP, TokenType.COMP_1,
                                                             TokenType.COMP_2, TokenType.COMP_3,
                                                             TokenType.SIGN, TokenType.VALUE,
                                                             TokenType.USAGE, TokenType.JUSTIFIED):
            if self._check(TokenType.IDENTIFIER):
                pic_parts.append(self._advance().value)
            elif self._check(TokenType.LPAREN):
                pic_parts.append(self._advance().value)
            elif self._check(TokenType.RPAREN):
                pic_parts.append(self._advance().value)
            elif self._check(TokenType.NUMERIC_LITERAL):
                pic_parts.append(self._advance().value)
            elif self._check(TokenType.LEVEL_NUMBER):
                # Handle numeric PIC characters like 9(10) - lexer tokenizes '9' as LEVEL_NUMBER
                pic_parts.append(self._advance().value)
            else:
                break

        return ''.join(pic_parts)

    def _peek(self) -> Token:
        """Peek at current token"""
        if self.current >= len(self.tokens):
            return self.tokens[-1]
        return self.tokens[self.current]

    def _advance(self) -> Token:
        """Consume and return current token"""
        if self.current < len(self.tokens):
            self.current += 1
        return self.tokens[self.current - 1]

    def _check(self, token_type: TokenType) -> bool:
        """Check if current token is of given type"""
        if self.current >= len(self.tokens):
            return False
        return self.tokens[self.current].type == token_type

    def _check_any(self, *token_types: TokenType) -> bool:
        """Check if current token matches any of the given types"""
        return any(self._check(tt) for tt in token_types)

    def _is_at_end(self) -> bool:
        """Check if we're at end of tokens"""
        return self.current >= len(self.tokens) or self.tokens[self.current].type == TokenType.EOF


def parse_copybook(filename: str) -> ASTNode:
    """
    Convenience function to parse a copybook file

    Args:
        filename: Path to copybook file

    Returns:
        Root AST node
    """
    lexer = CobolLexer(open(filename, 'r', encoding='utf-8').read())
    tokens = lexer.tokenize()

    parser = CobolParser(tokens)
    return parser.parse()


if __name__ == '__main__':
    import sys

    if len(sys.argv) > 1:
        ast = parse_copybook(sys.argv[1])

        def print_tree(node: ASTNode, indent: int = 0):
            """Print AST tree"""
            prefix = "  " * indent
            redef = f" REDEFINES {node.redefines}" if node.redefines else ""
            pic = f" PIC {node.picture.picture_string}" if node.picture else ""
            occurs = f" OCCURS {node.occurs}" if node.occurs else ""
            usage = f" {node.usage.usage_type}" if node.usage else ""

            print(f"{prefix}{node.level:02d} {node.name}{redef}{pic}{occurs}{usage}")

            for child in node.children:
                print_tree(child, indent + 1)

        print_tree(ast)
    else:
        print("Usage: python copybook_parser.py <copybook_file>")
