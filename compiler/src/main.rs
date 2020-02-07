use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;
use std::string::String;

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

#[derive(Debug)]
pub struct Parser<'a> {
    pub column: u32,
    pub line: u32,
    pub chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    pub keywords: HashMap<&'static str, bool>,
}

fn get_token<'a>(parser: &mut Parser<'a>, source_str: &'a str) -> Option<TokenData<'a>> {
    let mut skip_until_newline = false;
    let mut num_nested_comments: i32 = 0;
    let mut token = TokenData {
        column: parser.column,
        line: parser.line,
        value: "null",
        token_type: TokenType::Undefined,
    };

    loop {
        match parser.chars.next() {
            Some((pos, ch)) => {
                parser.column += 1;

                // ALWAYS increment the line number on newline characters
                if '\n' == ch {
                    parser.line += 1;
                    parser.column = 0;
                    skip_until_newline = false;
                }

                if skip_until_newline || ch.is_whitespace() {
                    continue;
                }

                if num_nested_comments > 0 {
                    if '*' == ch {
                        if let Some((_, ch)) = parser.chars.peek() {
                            if &'/' == ch {
                                parser.chars.next();
                                num_nested_comments -= 1;

                                assert!(num_nested_comments >= 0);
                            }
                        }
                    }
                    continue;
                }

                token.column = parser.column;
                token.line = parser.line;
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
                        if let Some((_, ch)) = parser.chars.peek() {
                            if &'.' == ch {
                                // Two dots in a sequence, it's the range operator
                                token.token_type = TokenType::Range;
                                parser.chars.next();
                                token_length += 1;
                            } else {
                                // A single dot is not a valid token, because floating point
                                // values are not supported.
                                token.token_type = TokenType::Undefined;
                            }
                        }
                    }
                    '/' => {
                        if let Some((_, ch)) = parser.chars.peek() {
                            if &'*' == ch || &'/' == ch {
                                // Commence comment
                                if &'*' == ch {
                                    num_nested_comments += 1;
                                } else {
                                    skip_until_newline = true;
                                }
                                parser.chars.next();
                                continue;
                            }
                            // Else it's divide op
                            token.token_type = TokenType::Operator;
                        }
                    }
                    ':' => {
                        token.token_type = TokenType::TypeSeparator;

                        if let Some((_, ch)) = parser.chars.peek() {
                            if &'=' == ch {
                                token.token_type = TokenType::Assignment;
                                parser.chars.next();
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
                        while let Some((_, ch)) = parser.chars.peek() {
                            if &'"' == ch && false == escape_next {
                                parser.chars.next();
                                token_length += 1;
                                break;
                            }

                            escape_next = &'\\' == ch;
                            parser.chars.next();
                            token_length += 1;
                        }
                    }
                    'A'..='z' => {
                        token.token_type = TokenType::Identifier;

                        while let Some((_, ch)) = parser.chars.peek() {
                            let token_str = &source_str[pos..pos + token_length];

                            if ch.is_alphanumeric() || &'_' == ch {
                                parser.chars.next();
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

                        while let Some((_, ch)) = parser.chars.peek() {
                            if ch.is_numeric() {
                                parser.chars.next();
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
                parser.column += (token_length - 1) as u32;

                if parser.keywords.contains_key(token.value) {
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

fn run() {
    let source_str = match read_program_to_string(env::args().collect()) {
        Ok(source_str) => source_str,
        Err(err) => {
            eprintln!("Application error: {}", err);
            process::exit(1);
        }
    };

    let mut parser = Parser {
        column: 0,
        line: 1,
        chars: source_str.char_indices().peekable(),
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
    };

    while let Some(token) = get_token(&mut parser, &source_str) {
        println!("{:#?}", token);
        assert!(TokenType::Undefined != token.token_type);
    }
}

fn main() {
    run();
}
