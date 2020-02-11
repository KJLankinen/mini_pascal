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
    TypeInt,
    TypeString,
    TypeBool,
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
    NumTokens,
}

// Token data
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
    token_map: HashMap<&'static str, TokenType>,           // Map for getting the type of a token
    source_str: &'a str,                                   // The entire source as a string slice
    current_token: Option<TokenData<'a>>,                  // The current token
    next_token: Option<TokenData<'a>>,                     // Next incoming token for peeking
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
            token_map: [
                ("var", TokenType::KeywordVar),
                ("for", TokenType::KeywordFor),
                ("end", TokenType::KeywordEnd),
                ("in", TokenType::KeywordIn),
                ("do", TokenType::KeywordDo),
                ("read", TokenType::KeywordRead),
                ("print", TokenType::KeywordPrint),
                ("assert", TokenType::KeywordAssert),
                ("int", TokenType::TypeInt),
                ("string", TokenType::TypeString),
                ("bool", TokenType::TypeBool),
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

    // Called by parser
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

    pub fn peek(&mut self) -> Option<&TokenData<'a>> {
        if self.next_token.is_none() {
            self.next_token = self.get_token().take();
        }

        self.next_token.as_ref()
    }

    pub fn current(&mut self) -> Option<&TokenData<'a>> {
        self.current_token.as_ref()
    }

    fn get_token(&mut self) -> Option<TokenData<'a>> {
        let mut skip_until_newline = false;
        let mut num_nested_comments: i32 = 0;
        let mut token = TokenData {
            column: self.column,
            line: self.line,
            value: "",
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

                    // Most tokens get their type from the map access at the bottom.
                    // This match disambiguates between some situations like comment vs divide op
                    // when '/' is encountered. Also longer tokens are handled by this match.
                    match ch {
                        '.' => {
                            // See if there's a second dot following the first
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'.' == ch {
                                    self.chars.next();
                                    token_length += 1;
                                }
                            }
                        }
                        '/' => {
                            // See whether or not a comment of one or another sort is starting
                            // If it's just the divide operator, it gets its type at the end
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
                            }
                        }
                        ':' => {
                            // Disambiguate between separator and assignment
                            // Type is received from map at the end
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'=' == ch {
                                    self.chars.next();
                                    token_length += 1;
                                }
                            }
                        }
                        '"' => {
                            token.token_type = TokenType::LiteralString;

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
                                if ch.is_alphanumeric() || &'_' == ch {
                                    self.chars.next();
                                    token_length += 1;
                                    continue;
                                }
                                break;
                            }
                        }
                        '0'..='9' => {
                            token.token_type = TokenType::LiteralInt;

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
            TokenType::EndOfProgram == self.scanner.next().unwrap().token_type,
            "{:?}",
            self.scanner.current().expect("Current token is None.")
        );
    }

    fn process_statement_list(&mut self) {
        let token = self.scanner.peek().expect("Next token is None.");
        // Stop if we've reached end of program or the end of 'for' block
        if TokenType::EndOfProgram != token.token_type && TokenType::KeywordEnd != token.token_type
        {
            self.process_statement();
            self.process_statement_list();
        }
    }

    fn process_statement(&mut self) {
        self.process_statement_prefix();
        self.process_end_of_statement();
    }

    fn process_statement_prefix(&mut self) {
        let token = self.scanner.next().unwrap();
        match token.token_type {
            TokenType::KeywordVar => {
                assert!(
                    TokenType::Identifier == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                assert!(
                    TokenType::TypeSeparator == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_type();

                if TokenType::Assignment
                    == self.scanner.peek().expect("Next token is None").token_type
                {
                    self.scanner.next();
                    self.process_expression();
                }
            }
            TokenType::KeywordFor => {
                assert!(
                    TokenType::Identifier == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                assert!(
                    TokenType::KeywordIn == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_expression();
                assert!(
                    TokenType::Range == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_expression();
                assert!(
                    TokenType::KeywordDo == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_statement_list();
                assert!(
                    TokenType::KeywordEnd == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                assert!(
                    TokenType::KeywordFor == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
            }
            TokenType::KeywordRead => {
                assert!(
                    TokenType::Identifier == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
            }
            TokenType::KeywordPrint => {
                self.process_expression();
            }
            TokenType::KeywordAssert => {
                assert!(
                    TokenType::LParen == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_expression();
                assert!(
                    TokenType::RParen == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
            }
            TokenType::Identifier => {
                assert!(
                    TokenType::Assignment == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_expression();
            }
            _ => assert!(
                false,
                "{:?}",
                self.scanner.current().expect("Current token is None.")
            ),
        }
    }

    fn process_end_of_statement(&mut self) {
        assert!(
            TokenType::EndOfStatement == self.scanner.next().unwrap().token_type,
            "{:?}",
            self.scanner.current().expect("Current token is None.")
        );
    }

    fn process_type(&mut self) {
        let token = self.scanner.next().unwrap();
        assert!(
            TokenType::TypeInt == token.token_type
                || TokenType::TypeBool == token.token_type
                || TokenType::TypeString == token.token_type,
            "{:?}",
            self.scanner.current().expect("Current token is None.")
        );
    }

    fn process_expression(&mut self) {
        // Starts with unary op
        if TokenType::OperatorNot == self.scanner.peek().expect("Next token is None").token_type {
            self.scanner.next().unwrap();
            self.process_operand();
        } else {
            // Either just a single operand or operand operator operand
            self.process_operand();
            let mut is_operator = true;
            match self.scanner.peek().expect("Next token is None").token_type {
                TokenType::OperatorPlus => {}
                TokenType::OperatorMinus => {}
                TokenType::OperatorMultiply => {}
                TokenType::OperatorDivide => {}
                TokenType::OperatorLessThan => {}
                TokenType::OperatorEqual => {}
                TokenType::OperatorAnd => {}
                _ => is_operator = false, // Not any operator
            }

            if is_operator {
                self.scanner.next();
                self.process_operand();
            }
        }
    }

    fn process_operand(&mut self) {
        let token = self.scanner.next().unwrap();
        if TokenType::LParen == token.token_type {
            self.process_expression();
            assert!(
                TokenType::RParen == self.scanner.next().unwrap().token_type,
                "{:?}",
                self.scanner.current().expect("Current token is None.")
            );
        } else {
            assert!(
                TokenType::LiteralInt == token.token_type
                    || TokenType::LiteralString == token.token_type
                    || TokenType::LiteralBool == token.token_type
                    || TokenType::Identifier == token.token_type,
                "{:?}",
                self.scanner.current().expect("Current token is None.")
            );
        }
    }
}
