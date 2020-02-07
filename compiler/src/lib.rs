use std::collections::HashMap;
use std::fs;

// Read the entire source code to a string and return that string
pub fn read_program_to_string(args: Vec<String>) -> Result<String, &'static str> {
    if 2 > args.len() {
        return Err("Provide the name of the file to compile.");
    }

    let filename: &str = &args[1];
    let contents = match fs::read_to_string(&filename) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Error with file {}: {}", filename, err);
            return Err("Problem with reading file");
        }
    };
    Ok(contents)
}

// Token types
// All the legal tokens of Mini PL.
// No disctinction between different operator overloads,
// that's for parser to decide.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Identifier,
    Keyword,
    BoolLiteral,
    IntLiteral,
    StrLiteral,
    Operator,
    Range,
    EndOfStatement,
    TypeSeparator,
    Assignment,
    Paren,
    Undefined,
    EndOfProgram,
    NumTokens,
}

// Token data
// What data we want to store for each token
#[derive(Debug, Clone, Copy)]
pub struct TokenData<'a> {
    pub column: u32, // How much white space from line start to start of this token
    pub line: u32,   // How many \n characters from file start
    pub token_type: TokenType, // Type of this token.
    pub value: &'a str, // The lexeme as a string slice
}

#[derive(Debug)]
pub struct Scanner<'a> {
    column: u32,                                           // Current column
    line: u32,                                             // Current line
    chars: std::iter::Peekable<std::str::CharIndices<'a>>, // Iterator over the source string
    keywords: HashMap<&'static str, bool>,                 // All the keywords for convenient check
    source_str: &'a str,                                   // The entire source as a string slice
    pub current_token: Option<TokenData<'a>>,              // The current token
    pub next_token: Option<TokenData<'a>>,                 // Next incoming token for peeking
}

impl<'a> Scanner<'a> {
    pub fn new(source_str: &'a str) -> Scanner<'a> {
        Scanner {
            column: 0,
            line: 1,
            chars: source_str.char_indices().peekable(),
            source_str: source_str,
            current_token: None,
            next_token: None,
            keywords: [
                ("var", true),
                ("for", true),
                ("end", true),
                ("in", true),
                ("do", true),
                ("read", true),
                ("print", true),
                ("int", true),
                ("string", true),
                ("bool", true),
                ("assert", true),
            ]
            .iter()
            .cloned()
            .collect(),
        }
    }

    // Update the current and next tokens
    // Called by parser
    pub fn step(&mut self) -> Option<&TokenData<'a>> {
        if self.current_token.is_none() {
            self.current_token = self.get_token().take();
            self.next_token = self.get_token().take();
        } else {
            self.current_token = self.next_token.take();
            self.next_token = self.get_token();
        }

        self.current_token.as_ref()
    }

    // Get the next token(s) to member variables
    fn get_token(&mut self) -> Option<TokenData<'a>> {
        let mut skip_until_newline = false;
        let mut num_nested_comments: i32 = 0;
        let mut token = TokenData {
            column: self.column,
            line: self.line,
            value: "\0",
            token_type: TokenType::Undefined,
        };

        loop {
            match self.chars.next() {
                Some((pos, ch)) => {
                    self.column += 1;

                    // ALWAYS increment the line number on newline characters
                    if '\n' == ch {
                        self.line += 1;
                        self.column = 0;
                        skip_until_newline = false;
                    }

                    if skip_until_newline || ch.is_whitespace() {
                        continue;
                    }

                    if num_nested_comments > 0 {
                        if '*' == ch {
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'/' == ch {
                                    self.chars.next();
                                    num_nested_comments -= 1;

                                    assert!(num_nested_comments >= 0);
                                }
                            }
                        }
                        continue;
                    }

                    token.column = self.column;
                    token.line = self.line;
                    token.token_type = TokenType::Undefined;
                    let mut token_length = 1;

                    match ch {
                        '+' | '-' | '*' | '<' | '=' | '&' | '!' => {
                            token.token_type = TokenType::Operator;
                        }
                        '(' | ')' => {
                            token.token_type = TokenType::Paren;
                        }
                        '.' => {
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'.' == ch {
                                    // Two dots in a sequence, it's the range operator
                                    token.token_type = TokenType::Range;
                                    self.chars.next();
                                    token_length += 1;
                                } else {
                                    // A single dot is not a valid token, because floating point
                                    // values are not supported.
                                    token.token_type = TokenType::Undefined;
                                }
                            }
                        }
                        '/' => {
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'*' == ch || &'/' == ch {
                                    // Commence comment
                                    if &'*' == ch {
                                        num_nested_comments += 1;
                                    } else {
                                        skip_until_newline = true;
                                    }
                                    self.chars.next();
                                    continue;
                                }
                                // Else it's divide op
                                token.token_type = TokenType::Operator;
                            }
                        }
                        ':' => {
                            token.token_type = TokenType::TypeSeparator;

                            if let Some((_, ch)) = self.chars.peek() {
                                if &'=' == ch {
                                    token.token_type = TokenType::Assignment;
                                    self.chars.next();
                                    token_length += 1;
                                }
                            }
                        }
                        ';' => {
                            token.token_type = TokenType::EndOfStatement;
                        }
                        '"' => {
                            token.token_type = TokenType::StrLiteral;

                            let mut escape_next = false;
                            while let Some((_, ch)) = self.chars.peek() {
                                if &'"' == ch && false == escape_next {
                                    self.chars.next();
                                    token_length += 1;
                                    break;
                                }

                                escape_next = &'\\' == ch;
                                self.chars.next();
                                token_length += 1;
                            }
                        }
                        'A'..='z' => {
                            token.token_type = TokenType::Identifier;

                            while let Some((_, ch)) = self.chars.peek() {
                                let token_str = &self.source_str[pos..pos + token_length];

                                if ch.is_alphanumeric() || &'_' == ch {
                                    self.chars.next();
                                    token_length += 1;
                                    continue;
                                } else if "true" == token_str || "false" == token_str {
                                    token.token_type = TokenType::BoolLiteral;
                                }
                                break;
                            }
                        }
                        '0'..='9' => {
                            token.token_type = TokenType::IntLiteral;

                            while let Some((_, ch)) = self.chars.peek() {
                                if ch.is_numeric() {
                                    self.chars.next();
                                    token_length += 1;
                                    continue;
                                }
                                break;
                            }
                        }
                        _ => {}
                    }

                    token.value = &self.source_str[pos..pos + token_length];
                    // We've already added 1 to the column at the start of the loop
                    self.column += (token_length - 1) as u32;

                    if self.keywords.contains_key(token.value) {
                        token.token_type = TokenType::Keyword;
                    }
                }
                None => {
                    token.token_type = TokenType::EndOfProgram;
                }
            }

            return Some(token);
        }
    }
}

pub struct Parser<'a> {
    pub scanner: Scanner<'a>,
}

// A recursive descent parser
impl<'a> Parser<'a> {
    pub fn parse(&mut self) {
        self.process_program();
    }

    fn process_program(&mut self) {
        self.process_statement_list();
        assert!(
            TokenType::EndOfProgram == self.scanner.step().unwrap().token_type,
            "{:?}",
            self.scanner.current_token.unwrap()
        );
    }

    fn process_statement_list(&mut self) {
        self.process_statement();
        self.process_end_of_statement();

        // Stop if we've reached end of program or the end of 'for' block
        if TokenType::EndOfProgram != self.scanner.next_token.unwrap().token_type
            && "end" != self.scanner.next_token.unwrap().value
        {
            self.process_statement_list();
        }
    }

    fn process_statement(&mut self) {
        let token = self.scanner.step().unwrap();
        match token.token_type {
            TokenType::Keyword => match token.value {
                "var" => {
                    assert!(
                        TokenType::Identifier == self.scanner.step().unwrap().token_type,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    assert!(
                        TokenType::TypeSeparator == self.scanner.step().unwrap().token_type,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    self.process_type();

                    if TokenType::Assignment == self.scanner.next_token.unwrap().token_type {
                        self.scanner.step();
                        self.process_expression();
                    }
                }
                "for" => {
                    assert!(
                        TokenType::Identifier == self.scanner.step().unwrap().token_type,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    assert!(
                        "in" == self.scanner.step().unwrap().value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    self.process_expression();
                    assert!(
                        TokenType::Range == self.scanner.step().unwrap().token_type,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    self.process_expression();
                    assert!(
                        "do" == self.scanner.step().unwrap().value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    self.process_statement_list();
                    assert!(
                        "end" == self.scanner.step().unwrap().value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    assert!(
                        "for" == self.scanner.step().unwrap().value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                }
                "read" => {
                    assert!(
                        TokenType::Identifier == self.scanner.step().unwrap().token_type,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                }
                "print" => {
                    self.process_expression();
                }
                "assert" => {
                    let token = self.scanner.step().unwrap();
                    assert!(
                        "(" == token.value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    self.process_expression();
                    let token = self.scanner.step().unwrap();
                    assert!(
                        ")" == token.value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                }
                _ => {
                    assert!(
                        false,
                        "Unhandled statement case {:?}",
                        self.scanner.current_token.unwrap()
                    );
                }
            },
            TokenType::Identifier => {
                let token = self.scanner.step().unwrap();
                assert!(
                    TokenType::Assignment == token.token_type,
                    "{:?}",
                    self.scanner.current_token.unwrap()
                );
                self.process_expression();
            }
            _ => assert!(false, "{:?}", self.scanner.current_token.unwrap()),
        }
    }

    fn process_end_of_statement(&mut self) {
        assert!(
            TokenType::EndOfStatement == self.scanner.step().unwrap().token_type,
            "{:?}",
            self.scanner.current_token.unwrap()
        );
    }

    fn process_type(&mut self) {
        let token = self.scanner.step().unwrap();
        assert!(
            TokenType::Keyword == token.token_type,
            "{:?}",
            self.scanner.current_token.unwrap()
        );
        assert!(
            "int" == token.value || "string" == token.value || "bool" == token.value,
            "{:?}",
            self.scanner.current_token.unwrap()
        );
    }

    fn process_expression(&mut self) {
        if TokenType::Operator == self.scanner.next_token.unwrap().token_type {
            let token = self.scanner.step().unwrap();
            assert!(
                "!" == token.value,
                "{:?}",
                self.scanner.current_token.unwrap()
            );
            self.process_operand();
        } else {
            self.process_operand();
            if TokenType::Operator == self.scanner.next_token.unwrap().token_type {
                let token = self.scanner.step().unwrap();
                // Need to take into account operator overloading
                // and suitable operands on both sides
                assert!(
                    TokenType::Operator == token.token_type,
                    "{:?}",
                    self.scanner.current_token.unwrap()
                );
                self.process_operand();
            }
        }
    }

    fn process_operand(&mut self) {
        let token = self.scanner.step().unwrap();
        if TokenType::Paren == token.token_type {
            assert!(
                "(" == token.value,
                "{:?}",
                self.scanner.current_token.unwrap()
            );
            self.process_expression();
            let token = self.scanner.step().unwrap();
            assert!(
                ")" == token.value,
                "{:?}",
                self.scanner.current_token.unwrap()
            );
        } else {
            assert!(
                TokenType::IntLiteral == token.token_type
                    || TokenType::StrLiteral == token.token_type
                    || TokenType::Identifier == token.token_type
                    || TokenType::BoolLiteral == token.token_type,
                "{:?}",
                self.scanner.current_token.unwrap()
            );
        }
    }
}
