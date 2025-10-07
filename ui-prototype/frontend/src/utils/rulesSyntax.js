// Monaco Editor language definition for Rules DSL

export const rulesLanguageDefinition = {
  // Language configuration
  languageConfiguration: {
    comments: {
      lineComment: '//',
      blockComment: ['/*', '*/'],
    },
    brackets: [
      ['{', '}'],
      ['[', ']'],
      ['(', ')'],
    ],
    autoClosingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '"', close: '"' },
      { open: "'", close: "'" },
    ],
    surroundingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '"', close: '"' },
      { open: "'", close: "'" },
    ],
  },

  // Monarch tokenizer
  monarchTokenizer: {
    defaultToken: 'invalid',
    tokenPostfix: '.rules',

    keywords: [
      'rule', 'if', 'then', 'elseif', 'else', 'endif', 'and', 'or', 'not',
      'true', 'false', 'null',
      'before', 'after', 'between', 'within',
      'contains', 'starts_with', 'ends_with', 'matches',
      'in', 'is_weekend', 'is_weekday', 'is_holiday',
    ],

    // MVP Core Functions (string, number processing, math)
    mvpFunctions: [
      'substring', 'length', 'round', 'percent', 'math'
    ],

    dateTimeFunctions: [
      'now', 'today', 'business_date',
      'year_of', 'month_of', 'day_of', 'hour_of', 'minute_of', 'second_of',
      'day_of_week', 'day_of_year', 'week_of_year',
      'age_years', 'age_months', 'age_days',
    ],

    actions: [
      'approveApplication', 'rejectApplication', 'conditionalApproval',
      'manualReview', 'instantApproval', 'requireManualReview',
      'flagForReview', 'sendAlert', 'approveTransaction',
    ],

    operators: [
      '=', '!=', '<', '<=', '>', '>=',
      '+', '-', '*', '/', '%',
    ],

    // The main tokenizer for our simple language
    tokenizer: {
      root: [
        // identifiers and keywords (support both lowercase and uppercase)
        [/[a-zA-Z_$][\w$]*/, {
          cases: {
            '@keywords': 'keyword',
            '@mvpFunctions': 'function.mvp',
            '@dateTimeFunctions': 'function',
            '@actions': 'function.action',
            '@default': 'identifier'
          }
        }],

        // whitespace
        { include: '@whitespace' },

        // delimiters and operators
        [/[{}()[\]]/, '@brackets'],
        [/[<>](?!@symbols)/, '@brackets'],
        [/@symbols/, {
          cases: {
            '@operators': 'operator',
            '@default': ''
          }
        }],

        // numbers
        [/\d*\.\d+([eE][-+]?\d+)?/, 'number.float'],
        [/\d+/, 'number'],

        // delimiter: after number because of .\d floats
        [/[;,.]/, 'delimiter'],

        // Double-quoted strings: for attributes with special characters (blue color)
        [/"([^"\\]|\\.)*$/, 'string.invalid'],  // non-terminated double-quoted string
        [/"/, { token: 'string.dquote', bracket: '@open', next: '@dquotedstring' }],

        // Single-quoted strings: for string literals/constants (green color)
        [/'([^'\\]|\\.)*$/, 'string.invalid'],  // non-terminated single-quoted string
        [/'/, { token: 'string.squote', bracket: '@open', next: '@squotedstring' }],

        // date literals
        [/\d{4}-\d{2}-\d{2}(T\d{2}:\d{2}:\d{2}(Z|[+-]\d{2}:\d{2})?)?/, 'string.date'],

        // time units
        [/\b(years?|months?|weeks?|days?|hours?|minutes?|seconds?)\b/, 'keyword.time'],
      ],

      comment: [
        [/[^/*]+/, 'comment'],
        [/\/\*/, 'comment', '@push'],    // nested comment
        ["\\*/", 'comment', '@pop'],
        [/[/*]/, 'comment']
      ],

      dquotedstring: [
        [/[^\\"]+/, 'string.attribute'],
        [/\\./, 'string.escape.invalid'],
        [/"/, { token: 'string.dquote', bracket: '@close', next: '@pop' }]
      ],

      squotedstring: [
        [/[^\\']+/, 'string.literal'],
        [/\\./, 'string.escape.invalid'],
        [/'/, { token: 'string.squote', bracket: '@close', next: '@pop' }]
      ],

      whitespace: [
        [/[ \t\r\n]+/, 'white'],
        [/\/\*/, 'comment', '@comment'],
        [/\/\/.*$/, 'comment'],
      ],
    },

    symbols: /[=><!~?:&|+\-*/^%]+/,
  },

  // Theme definition
  theme: {
    base: 'vs',
    inherit: true,
    rules: [
      { token: 'keyword', foreground: '0000ff', fontStyle: 'bold' },
      { token: 'function.mvp', foreground: 'e91e63', fontStyle: 'bold' },
      { token: 'function', foreground: '795e26' },
      { token: 'function.action', foreground: '008000', fontStyle: 'bold' },
      { token: 'string.attribute', foreground: '0451a5' },  // Double-quoted: blue for attributes
      { token: 'string.literal', foreground: '008000' },    // Single-quoted: green for string literals
      { token: 'string.date', foreground: '098658' },
      { token: 'number', foreground: '098658' },
      { token: 'comment', foreground: '008000', fontStyle: 'italic' },
      { token: 'operator', foreground: '000000' },
      { token: 'keyword.time', foreground: '0451a5' },
    ],
    colors: {
      'editor.foreground': '#000000',
      'editor.background': '#ffffff',
      'editorCursor.foreground': '#000000',
      'editor.lineHighlightBackground': '#f5f5f5',
      'editorLineNumber.foreground': '#999999',
    }
  },

  // Completion item provider (will be enhanced with server-side data)
  completionItemProvider: {
    provideCompletionItems: (model, position) => {
      // Basic static suggestions - these will be enhanced with server-side data
      const suggestions = [
        {
          label: 'rule',
          kind: 14, // monaco.languages.CompletionItemKind.Keyword
          // eslint-disable-next-line no-template-curly-in-string
          insertText: 'rule ${1:ruleName}:\n    if ${2:condition} then ${3:action}',
          insertTextRules: 4, // monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
          documentation: 'Define a new rule'
        },
        {
          label: 'if',
          kind: 14,
          // eslint-disable-next-line no-template-curly-in-string
          insertText: 'if ${1:condition} then\n    ${2:action}\nelseif ${3:condition} then\n    ${4:action}\nelse\n    ${5:action}\nendif',
          insertTextRules: 4,
          documentation: 'Conditional statement with elseif/else/endif'
        },
        {
          label: 'elseif',
          kind: 14,
          // eslint-disable-next-line no-template-curly-in-string
          insertText: 'elseif ${1:condition} then\n    ${2:action}',
          insertTextRules: 4,
          documentation: 'Additional conditional branch'
        },
        {
          label: 'endif',
          kind: 14,
          insertText: 'endif',
          documentation: 'End of conditional block'
        },
        // MVP Functions
        {
          label: 'substring',
          kind: 3, // monaco.languages.CompletionItemKind.Function
          // eslint-disable-next-line no-template-curly-in-string
          insertText: 'substring(${1:text}, ${2:start}, ${3:length})',
          insertTextRules: 4,
          documentation: 'Extract substring from text starting at position with specified length'
        },
        {
          label: 'length',
          kind: 3,
          // eslint-disable-next-line no-template-curly-in-string
          insertText: 'length(${1:text})',
          insertTextRules: 4,
          documentation: 'Get length of text string'
        },
        {
          label: 'round',
          kind: 3,
          // eslint-disable-next-line no-template-curly-in-string
          insertText: 'round(${1:number}, ${2:decimals})',
          insertTextRules: 4,
          documentation: 'Round number to specified decimal places'
        },
        {
          label: 'percent',
          kind: 3,
          // eslint-disable-next-line no-template-curly-in-string
          insertText: 'percent(${1:part}, ${2:whole})',
          insertTextRules: 4,
          documentation: 'Calculate percentage of part relative to whole'
        },
        {
          label: 'math',
          kind: 3,
          // eslint-disable-next-line no-template-curly-in-string
          insertText: 'math("${1:expression}")',
          insertTextRules: 4,
          documentation: 'High-performance math expression evaluation (e.g., "applicant.income * 0.3")'
        },
        // More suggestions will be loaded dynamically from the server
      ];

      return { suggestions };
    }
  }
};

// Helper function to convert server suggestion kind to Monaco kind
export const getMonacoCompletionKind = (kind, monaco) => {
  if (!monaco) return 1;

  switch (kind) {
    case 'keyword': return monaco.languages.CompletionItemKind.Keyword;
    case 'property': return monaco.languages.CompletionItemKind.Property;
    case 'function': return monaco.languages.CompletionItemKind.Function;
    case 'constant': return monaco.languages.CompletionItemKind.Constant;
    case 'operator': return monaco.languages.CompletionItemKind.Operator;
    default: return monaco.languages.CompletionItemKind.Text;
  }
};