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
      'rule', 'if', 'then', 'else', 'and', 'or', 'not',
      'true', 'false', 'null',
      'before', 'after', 'between', 'within',
      'contains', 'starts_with', 'ends_with', 'matches',
      'in', 'is_weekend', 'is_weekday', 'is_holiday',
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
        // identifiers and keywords
        [/[a-z_$][\w$]*/, {
          cases: {
            '@keywords': 'keyword',
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

        // strings
        [/"([^"\\]|\\.)*$/, 'string.invalid'],  // non-terminated string
        [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

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

      string: [
        [/[^\\"]+/, 'string'],
        [/\\./, 'string.escape.invalid'],
        [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
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
      { token: 'function', foreground: '795e26' },
      { token: 'function.action', foreground: '008000', fontStyle: 'bold' },
      { token: 'string', foreground: 'a31515' },
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
          insertText: 'if ${1:condition} then ${2:action}',
          insertTextRules: 4,
          documentation: 'Conditional statement'
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