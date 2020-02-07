use std::collections::HashMap;
use std::fs;

// =====================================================
// Token types
// =====================================================
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

// =====================================================
// Token data
// =====================================================
// What data we want to store for each token
#[derive(Debug, Clone, Copy)]
pub struct TokenData<'a> {
    pub column: u32, // How much white space from line start to start of this token
    pub line: u32,   // How many \n characters from file start
    pub token_type: TokenType, // What is the type of this token.
    pub value: &'a str,
}

#[derive(Debug)]
pub struct Scanner<'a> {
    pub column: u32,
    pub line: u32,
    pub chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    pub keywords: HashMap<&'static str, bool>,
    pub source_str: &'a str,
    pub current_token: Option<TokenData<'a>>,
    pub next_token: Option<TokenData<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn step(&mut self) {
        if self.current_token.is_none() {
            self.current_token = self.get_token().take();
            self.next_token = self.get_token().take();
        } else {
            self.current_token = self.next_token.take();
            self.next_token = self.get_token();
        }
    }

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
