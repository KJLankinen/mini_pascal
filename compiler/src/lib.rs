use std::collections::HashMap;

// =====================================================
// Token types
// =====================================================
// All the legal tokens of Mini PL.
// No disctinction between different operator overloads,
// that's for scanner.to decide.
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
}

pub fn get_token<'a>(scanner: &mut Scanner<'a>, source_str: &'a str) -> Option<TokenData<'a>> {
    let mut skip_until_newline = false;
    let mut num_nested_comments: i32 = 0;
    let mut token = TokenData {
        column: scanner.column,
        line: scanner.line,
        value: "null",
        token_type: TokenType::Undefined,
    };

    loop {
        match scanner.chars.next() {
            Some((pos, ch)) => {
                scanner.column += 1;

                // ALWAYS increment the line number on newline characters
                if '\n' == ch {
                    scanner.line += 1;
                    scanner.column = 0;
                    skip_until_newline = false;
                }

                if skip_until_newline || ch.is_whitespace() {
                    continue;
                }

                if num_nested_comments > 0 {
                    if '*' == ch {
                        if let Some((_, ch)) = scanner.chars.peek() {
                            if &'/' == ch {
                                scanner.chars.next();
                                num_nested_comments -= 1;

                                assert!(num_nested_comments >= 0);
                            }
                        }
                    }
                    continue;
                }

                token.column = scanner.column;
                token.line = scanner.line;
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
                        if let Some((_, ch)) = scanner.chars.peek() {
                            if &'.' == ch {
                                // Two dots in a sequence, it's the range operator
                                token.token_type = TokenType::Range;
                                scanner.chars.next();
                                token_length += 1;
                            } else {
                                // A single dot is not a valid token, because floating point
                                // values are not supported.
                                token.token_type = TokenType::Undefined;
                            }
                        }
                    }
                    '/' => {
                        if let Some((_, ch)) = scanner.chars.peek() {
                            if &'*' == ch || &'/' == ch {
                                // Commence comment
                                if &'*' == ch {
                                    num_nested_comments += 1;
                                } else {
                                    skip_until_newline = true;
                                }
                                scanner.chars.next();
                                continue;
                            }
                            // Else it's divide op
                            token.token_type = TokenType::Operator;
                        }
                    }
                    ':' => {
                        token.token_type = TokenType::TypeSeparator;

                        if let Some((_, ch)) = scanner.chars.peek() {
                            if &'=' == ch {
                                token.token_type = TokenType::Assignment;
                                scanner.chars.next();
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
                        while let Some((_, ch)) = scanner.chars.peek() {
                            if &'"' == ch && false == escape_next {
                                scanner.chars.next();
                                token_length += 1;
                                break;
                            }

                            escape_next = &'\\' == ch;
                            scanner.chars.next();
                            token_length += 1;
                        }
                    }
                    'A'..='z' => {
                        token.token_type = TokenType::Identifier;

                        while let Some((_, ch)) = scanner.chars.peek() {
                            let token_str = &source_str[pos..pos + token_length];

                            if ch.is_alphanumeric() || &'_' == ch {
                                scanner.chars.next();
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

                        while let Some((_, ch)) = scanner.chars.peek() {
                            if ch.is_numeric() {
                                scanner.chars.next();
                                token_length += 1;
                                continue;
                            }
                            break;
                        }
                    }
                    _ => {}
                }

                token.value = &source_str[pos..pos + token_length];
                // We've already added 1 to the column at the start of the loop
                scanner.column += (token_length - 1) as u32;

                if scanner.keywords.contains_key(token.value) {
                    token.token_type = TokenType::Keyword;
                }

                return Some(token);
            }
            None => {
                return None;
            }
        }
    }
}
