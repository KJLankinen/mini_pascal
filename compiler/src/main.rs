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
#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    Identifier,
    Keyword,
    BoolLiteral,
    IntLiteral,
    StrLiteral,
    Assignment,
    Operator,
    Punctuation,
    Undefined,
    Invalid,
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
    if args.len() < 2 {
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
        parser.column += 1;

        match parser.chars.next() {
            Some((pos, ch)) => {
                // ALWAYS increment the line number on newline characters
                if ch == '\n' {
                    parser.line += 1;
                    parser.column = 0;
                    skip_until_newline = false;
                }

                if skip_until_newline || ch.is_whitespace() {
                    continue;
                }

                if num_nested_comments > 0 {
                    if ch == '*' {
                        if let Some((_, ch)) = parser.chars.peek() {
                            if ch == &'/' {
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
                token.value = &source_str[pos..pos + 1];
                token.token_type = TokenType::Undefined;

                match ch {
                    '+' | '-' | '*' | '=' | '<' | '&' | '!' | '.' | '/' => {
                        token.token_type = TokenType::Operator;

                        if ch == '.' {
                            if let Some((_, ch)) = parser.chars.peek() {
                                if ch == &'.' {
                                    // Two dots in a sequence, it's the range operator
                                    parser.chars.next();
                                    token.value = &source_str[pos..pos + 2];
                                } else {
                                    // A single dot is not a valid token, because floating point
                                    // values are not supported. Return an invalid token.
                                    token.token_type = TokenType::Invalid;
                                }
                            }
                        } else if ch == '/' {
                            if let Some((_, ch)) = parser.chars.peek() {
                                if ch == &'*' || ch == &'/' {
                                    // Commence comment
                                    if ch == &'*' {
                                        num_nested_comments += 1;
                                    } else {
                                        skip_until_newline = true;
                                    }
                                    parser.chars.next();
                                    continue;
                                }
                            }
                        }

                        return Some(token);
                    }
                    _ => println!("Unhandled case for character \"{}\"", ch),
                }
            }
            None => {
                return None;
            }
        }
    }
}

fn run() {
    // Must stay alive until the end of the interpreter
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
    }
}

fn main() {
    run();
}
