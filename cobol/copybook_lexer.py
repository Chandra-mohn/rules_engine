"""
COBOL Copybook Lexer
Tokenizes COBOL copybook definitions for parsing.
"""

import re
from enum import Enum, auto
from dataclasses import dataclass
from typing import List, Optional


class TokenType(Enum):
    """Token types for COBOL copybook parsing"""
    # Level numbers
    LEVEL_NUMBER = auto()

    # Identifiers and literals
    IDENTIFIER = auto()
    STRING_LITERAL = auto()
    NUMERIC_LITERAL = auto()

    # Keywords
    PIC = auto()
    PICTURE = auto()
    OCCURS = auto()
    TIMES = auto()
    REDEFINES = auto()
    COMP = auto()
    COMP_1 = auto()
    COMP_2 = auto()
    COMP_3 = auto()
    COMP_4 = auto()
    COMPUTATIONAL = auto()
    COMPUTATIONAL_1 = auto()
    COMPUTATIONAL_2 = auto()
    COMPUTATIONAL_3 = auto()
    COMPUTATIONAL_4 = auto()
    SIGN = auto()
    LEADING = auto()
    TRAILING = auto()
    SEPARATE = auto()
    CHARACTER = auto()
    JUSTIFIED = auto()
    JUST = auto()
    RIGHT = auto()
    BLANK = auto()
    WHEN = auto()
    ZERO = auto()
    VALUE = auto()
    IS = auto()
    DEPENDING = auto()
    ON = auto()
    SYNCHRONIZED = auto()
    SYNC = auto()
    LEFT = auto()
    USAGE = auto()
    BINARY = auto()
    PACKED_DECIMAL = auto()
    DISPLAY = auto()
    INDEX = auto()
    POINTER = auto()

    # Symbols
    PERIOD = auto()
    LPAREN = auto()
    RPAREN = auto()

    # Special
    COMMENT = auto()
    EOF = auto()
    NEWLINE = auto()


@dataclass
class Token:
    """Represents a single token"""
    type: TokenType
    value: str
    line: int
    column: int


class CobolLexer:
    """Lexical analyzer for COBOL copybooks"""

    # Keyword mapping (case-insensitive)
    KEYWORDS = {
        'PIC': TokenType.PIC,
        'PICTURE': TokenType.PICTURE,
        'OCCURS': TokenType.OCCURS,
        'TIMES': TokenType.TIMES,
        'REDEFINES': TokenType.REDEFINES,
        'COMP': TokenType.COMP,
        'COMP-1': TokenType.COMP_1,
        'COMP-2': TokenType.COMP_2,
        'COMP-3': TokenType.COMP_3,
        'COMP-4': TokenType.COMP_4,
        'COMPUTATIONAL': TokenType.COMPUTATIONAL,
        'COMPUTATIONAL-1': TokenType.COMPUTATIONAL_1,
        'COMPUTATIONAL-2': TokenType.COMPUTATIONAL_2,
        'COMPUTATIONAL-3': TokenType.COMPUTATIONAL_3,
        'COMPUTATIONAL-4': TokenType.COMPUTATIONAL_4,
        'SIGN': TokenType.SIGN,
        'LEADING': TokenType.LEADING,
        'TRAILING': TokenType.TRAILING,
        'SEPARATE': TokenType.SEPARATE,
        'CHARACTER': TokenType.CHARACTER,
        'JUSTIFIED': TokenType.JUSTIFIED,
        'JUST': TokenType.JUST,
        'RIGHT': TokenType.RIGHT,
        'BLANK': TokenType.BLANK,
        'WHEN': TokenType.WHEN,
        'ZERO': TokenType.ZERO,
        'VALUE': TokenType.VALUE,
        'IS': TokenType.IS,
        'DEPENDING': TokenType.DEPENDING,
        'ON': TokenType.ON,
        'SYNCHRONIZED': TokenType.SYNCHRONIZED,
        'SYNC': TokenType.SYNC,
        'LEFT': TokenType.LEFT,
        'USAGE': TokenType.USAGE,
        'BINARY': TokenType.BINARY,
        'PACKED-DECIMAL': TokenType.PACKED_DECIMAL,
        'DISPLAY': TokenType.DISPLAY,
        'INDEX': TokenType.INDEX,
        'POINTER': TokenType.POINTER,
    }

    def __init__(self, text: str, fixed_format: bool = True):
        """
        Initialize lexer

        Args:
            text: COBOL copybook source text
            fixed_format: True for fixed-format COBOL (columns 1-6 for sequence,
                         7 for indicator, 8-72 for code), False for free-format
        """
        self.text = text
        self.fixed_format = fixed_format
        self.lines = text.split('\n')
        self.current_line = 0
        self.current_column = 0
        self.tokens: List[Token] = []

    def tokenize(self) -> List[Token]:
        """Tokenize the entire copybook"""
        for line_num, line in enumerate(self.lines, start=1):
            self._tokenize_line(line, line_num)

        # Add EOF token
        self.tokens.append(Token(
            type=TokenType.EOF,
            value='',
            line=len(self.lines),
            column=0
        ))

        return self.tokens

    def _tokenize_line(self, line: str, line_num: int):
        """Tokenize a single line"""
        # Handle fixed format COBOL
        if self.fixed_format:
            if len(line) < 7:
                return  # Skip short lines

            indicator = line[6] if len(line) > 6 else ' '

            # Handle comment lines
            if indicator in ('*', '/'):
                self.tokens.append(Token(
                    type=TokenType.COMMENT,
                    value=line[7:72].rstrip() if len(line) > 7 else '',
                    line=line_num,
                    column=7
                ))
                return

            # Extract code area (columns 8-72)
            code = line[7:72] if len(line) > 7 else ''
        else:
            code = line

        # Remove inline comments (starting with *)
        if '*' in code and code.strip().startswith('*'):
            self.tokens.append(Token(
                type=TokenType.COMMENT,
                value=code.strip()[1:].strip(),
                line=line_num,
                column=0
            ))
            return

        # Tokenize the code area
        self._scan_code(code, line_num)

    def _scan_code(self, code: str, line_num: int):
        """Scan code area for tokens"""
        pos = 0
        column_offset = 8 if self.fixed_format else 1

        while pos < len(code):
            # Skip whitespace
            if code[pos].isspace():
                pos += 1
                continue

            # Period
            if code[pos] == '.':
                self.tokens.append(Token(
                    type=TokenType.PERIOD,
                    value='.',
                    line=line_num,
                    column=column_offset + pos
                ))
                pos += 1
                continue

            # Parentheses
            if code[pos] == '(':
                self.tokens.append(Token(
                    type=TokenType.LPAREN,
                    value='(',
                    line=line_num,
                    column=column_offset + pos
                ))
                pos += 1
                continue

            if code[pos] == ')':
                self.tokens.append(Token(
                    type=TokenType.RPAREN,
                    value=')',
                    line=line_num,
                    column=column_offset + pos
                ))
                pos += 1
                continue

            # String literals (quotes)
            if code[pos] in ('"', "'"):
                token, new_pos = self._scan_string(code, pos, line_num, column_offset)
                self.tokens.append(token)
                pos = new_pos
                continue

            # Numbers (level numbers or numeric literals)
            if code[pos].isdigit():
                token, new_pos = self._scan_number(code, pos, line_num, column_offset)
                self.tokens.append(token)
                pos = new_pos
                continue

            # Identifiers and keywords
            if code[pos].isalpha() or code[pos] in ('-', '_'):
                token, new_pos = self._scan_identifier(code, pos, line_num, column_offset)
                self.tokens.append(token)
                pos = new_pos
                continue

            # PIC clause special characters (X, 9, A, S, V, etc.)
            # These are handled as part of identifiers

            # Unknown character, skip it
            pos += 1

    def _scan_string(self, code: str, start: int, line_num: int, column_offset: int) -> tuple[Token, int]:
        """Scan a string literal"""
        quote = code[start]
        pos = start + 1
        value = ''

        while pos < len(code):
            if code[pos] == quote:
                # Check for doubled quote (escape)
                if pos + 1 < len(code) and code[pos + 1] == quote:
                    value += quote
                    pos += 2
                else:
                    pos += 1
                    break
            else:
                value += code[pos]
                pos += 1

        return Token(
            type=TokenType.STRING_LITERAL,
            value=value,
            line=line_num,
            column=column_offset + start
        ), pos

    def _scan_number(self, code: str, start: int, line_num: int, column_offset: int) -> tuple[Token, int]:
        """Scan a numeric literal or level number"""
        pos = start
        value = ''

        # Scan digits
        while pos < len(code) and (code[pos].isdigit() or code[pos] in ('.', '+', '-')):
            value += code[pos]
            pos += 1

        # Check if this is a level number (01-49, 66, 77, 88) at start of statement
        # Level numbers are typically 2 digits
        if len(value) <= 2 and value.isdigit():
            level = int(value)
            if 1 <= level <= 49 or level in (66, 77, 88):
                return Token(
                    type=TokenType.LEVEL_NUMBER,
                    value=value,
                    line=line_num,
                    column=column_offset + start
                ), pos

        return Token(
            type=TokenType.NUMERIC_LITERAL,
            value=value,
            line=line_num,
            column=column_offset + start
        ), pos

    def _scan_identifier(self, code: str, start: int, line_num: int, column_offset: int) -> tuple[Token, int]:
        """Scan an identifier or keyword"""
        pos = start
        value = ''

        # Scan alphanumeric, hyphens, and underscores
        while pos < len(code) and (code[pos].isalnum() or code[pos] in ('-', '_')):
            value += code[pos]
            pos += 1

        # Check if it's a keyword
        upper_value = value.upper()
        token_type = self.KEYWORDS.get(upper_value, TokenType.IDENTIFIER)

        return Token(
            type=token_type,
            value=value,
            line=line_num,
            column=column_offset + start
        ), pos


def tokenize_copybook(filename: str, fixed_format: bool = True) -> List[Token]:
    """
    Convenience function to tokenize a copybook file

    Args:
        filename: Path to copybook file
        fixed_format: True for fixed-format COBOL

    Returns:
        List of tokens
    """
    with open(filename, 'r', encoding='utf-8') as f:
        text = f.read()

    lexer = CobolLexer(text, fixed_format)
    return lexer.tokenize()


if __name__ == '__main__':
    # Test the lexer
    import sys

    if len(sys.argv) > 1:
        tokens = tokenize_copybook(sys.argv[1])
        for token in tokens[:50]:  # Print first 50 tokens
            print(f"{token.line}:{token.column} {token.type.name:20} {token.value}")
    else:
        print("Usage: python copybook_lexer.py <copybook_file>")
