use std::collections::HashMap;

// Token types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Identifier,
    KeywordVar,
    KeywordFor,
    KeywordEnd,
    KeywordIn,
    KeywordDo,
    KeywordRead,
    KeywordPrint,
    KeywordAssert,
    Type,
    LiteralBool,
    LiteralInt,
    LiteralString,
    OperatorPlus,
    OperatorMinus,
    OperatorMultiply,
    OperatorDivide,
    OperatorLessThan,
    OperatorEqual,
    OperatorAnd,
    OperatorNot,
    Range,
    EndOfStatement,
    TypeSeparator,
    Assignment,
    LParen,
    RParen,
    Undefined,
    EndOfProgram,
}

// Token data
#[derive(Debug, Clone, Copy)]
pub struct TokenData<'a> {
    pub column: u32, // How much white space from line start to start of this token
    pub line: u32,   // How many \n characters from file start
    pub token_type: TokenType, // Type of this token.
    pub value: &'a str, // The lexeme as a string slice
}

impl<'a> Default for TokenData<'a> {
    fn default() -> TokenData<'a> {
        TokenData {
            column: 0,
            line: 0,
            token_type: TokenType::Undefined,
            value: "",
        }
    }
}

#[derive(Debug)]
pub struct Scanner<'a> {
    column: u32, // Current column
    line: u32,   // Current line
    skip_until_newline: bool,
    token_length: usize,
    previous_newline: usize,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>, // Iterator over the source string
    lines: Vec<&'a str>, // All the lines of code for error messaging
    token_map: HashMap<&'static str, TokenType>, // Map for getting the type of a token
    source_str: &'a str, // The entire source as a string slice
    current_token: Option<TokenData<'a>>, // The current token
    next_token: Option<TokenData<'a>>, // Next incoming token for peeking
}

impl<'a> Scanner<'a> {
    pub fn new(source_str: &'a str) -> Scanner<'a> {
        Scanner {
            column: 0,
            line: 1,
            skip_until_newline: false,
            token_length: 0,
            previous_newline: 0,
            chars: source_str.char_indices().peekable(),
            lines: vec![],
            source_str: source_str,
            current_token: None,
            next_token: None,
            token_map: [
                ("var", TokenType::KeywordVar),
                ("for", TokenType::KeywordFor),
                ("end", TokenType::KeywordEnd),
                ("in", TokenType::KeywordIn),
                ("do", TokenType::KeywordDo),
                ("read", TokenType::KeywordRead),
                ("print", TokenType::KeywordPrint),
                ("assert", TokenType::KeywordAssert),
                ("int", TokenType::Type),
                ("string", TokenType::Type),
                ("bool", TokenType::Type),
                ("(", TokenType::LParen),
                (")", TokenType::RParen),
                (";", TokenType::EndOfStatement),
                ("+", TokenType::OperatorPlus),
                ("-", TokenType::OperatorMinus),
                ("*", TokenType::OperatorMultiply),
                ("/", TokenType::OperatorDivide),
                ("<", TokenType::OperatorLessThan),
                ("=", TokenType::OperatorEqual),
                ("&", TokenType::OperatorAnd),
                ("!", TokenType::OperatorNot),
                ("..", TokenType::Range),
                (":", TokenType::TypeSeparator),
                (":=", TokenType::Assignment),
                ("true", TokenType::LiteralBool),
                ("false", TokenType::LiteralBool),
            ]
            .iter()
            .cloned()
            .collect(),
        }
    }

    pub fn print_line(&mut self, line: usize, column: usize) {
        assert!(line < self.lines.len(), "Line number is too large.");
        println!("\n\"{}\"", self.lines[line]);
        let arrow: String = vec!['-'; column + 1].into_iter().collect();
        println!("{}\n", arrow + "^---this");
    }

    // Consume the previous token and take a look at the next
    pub fn next(&mut self) -> Option<&TokenData<'a>> {
        if self.next_token.is_none() {
            self.current_token = self.get_token().take();
            self.next_token = self.get_token().take();
        } else {
            self.current_token = self.next_token.take();
            self.next_token = self.get_token().take();
        }

        self.current_token.as_ref()
    }

    // Peek at the next token without consuming anything
    pub fn peek(&mut self) -> Option<&TokenData<'a>> {
        if self.next_token.is_none() {
            self.next_token = self.get_token().take();
        }

        self.next_token.as_ref()
    }

    fn get_char(&mut self) -> Option<(usize, char)> {
        match self.chars.peek() {
            Some((pos, ch)) => {
                self.column += 1;
                self.token_length += 1;
                if &'\n' == ch {
                    self.line += 1;
                    self.column = 0;
                    self.skip_until_newline = false;
                    self.lines
                        .push(&self.source_str[self.previous_newline..*pos]);
                    self.previous_newline = *pos + 1;
                }
                self.chars.next()
            }
            None => None,
        }
    }

    fn skipped_comment(&mut self) -> bool {
        // Enter this function with a single '/'. Peek ahead, and if a comment is starting, skip
        // it. If not, return false and let the scanner continue.
        match self.chars.peek() {
            Some((_, ch)) => {
                match ch {
                    &'/' => {
                        // Singe line comment
                        self.skip_until_newline = true;
                        while self.skip_until_newline {
                            self.get_char();
                        }
                    }
                    &'*' => {
                        // Multi line comment
                        let mut depth: i32 = 1;
                        self.get_char();
                        while let Some((_, ch)) = self.get_char() {
                            match ch {
                                '*' => {
                                    if let Some((_, ch)) = self.chars.peek() {
                                        if &'/' == ch {
                                            self.get_char();
                                            depth -= 1;
                                        }
                                    }
                                }
                                '/' => {
                                    if let Some((_, ch)) = self.chars.peek() {
                                        if &'*' == ch {
                                            self.get_char();
                                            depth += 1;
                                        }
                                    }
                                }
                                _ => {}
                            }

                            if 0 == depth {
                                break;
                            }

                            assert!(depth > 0);
                        }
                    }
                    _ => return false,
                }
                true
            }
            None => false,
        }
    }

    fn get_token(&mut self) -> Option<TokenData<'a>> {
        let mut token = TokenData {
            column: self.column,
            line: self.line,
            value: "",
            token_type: TokenType::Undefined,
        };

        loop {
            match self.get_char() {
                Some((pos, ch)) => {
                    if ch.is_whitespace() {
                        continue;
                    }

                    token.column = self.column;
                    token.line = self.line;
                    self.token_length = 1;

                    // Most tokens get their type from the map access at the bottom.
                    // This match disambiguates between some situations like comment vs divide op
                    // when '/' is encountered. Also longer tokens are handled by this match.
                    match ch {
                        '.' => {
                            // See if there's a second dot following the first
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'.' == ch {
                                    self.get_char();
                                }
                            }
                        }
                        '/' => {
                            if self.skipped_comment() {
                                continue;
                            }
                        }
                        ':' => {
                            // Disambiguate between separator and assignment
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'=' == ch {
                                    self.get_char();
                                }
                            }
                        }
                        '"' => {
                            token.token_type = TokenType::LiteralString;
                            let mut escape_next = false;

                            while let Some((_, ch)) = self.chars.peek() {
                                let is_finished = &'"' == ch && false == escape_next;
                                escape_next = &'\\' == ch;
                                self.get_char();

                                if is_finished {
                                    break;
                                }
                            }
                        }
                        'A'..='z' => {
                            token.token_type = TokenType::Identifier;

                            while let Some((_, ch)) = self.chars.peek() {
                                if ch.is_alphanumeric() || &'_' == ch {
                                    self.get_char();
                                    continue;
                                }
                                break;
                            }
                        }
                        '0'..='9' => {
                            token.token_type = TokenType::LiteralInt;

                            while let Some((_, ch)) = self.chars.peek() {
                                if ch.is_numeric() {
                                    self.get_char();
                                    continue;
                                }
                                break;
                            }
                        }
                        _ => {}
                    }

                    token.value = &self.source_str[pos..pos + self.token_length];

                    // Get your type here, if you're in the map
                    if let Some(tt) = self.token_map.get(token.value) {
                        token.token_type = *tt;
                    }
                }
                None => {
                    // Out of characters
                    token.value = "\0";
                    token.token_type = TokenType::EndOfProgram;
                }
            }

            return Some(token);
        }
    }
}
