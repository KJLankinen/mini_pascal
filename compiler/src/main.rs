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

impl<'a> TokenData<'a> {
    fn new(column: u32, line: u32, token_type: TokenType, value: &'a str) -> TokenData<'a> {
        TokenData {
            column: column,
            line: line,
            value: value,
            token_type: token_type,
        }
    }
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

fn run() {
    // The value is essentially useless, because the map will never have any false entries.
    let keywords: HashMap<&str, bool> = [
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
    .collect();

    let contents = match read_program_to_string(env::args().collect()) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Application error: {}", err);
            process::exit(1);
        }
    };

    let mut chars = contents.char_indices().peekable();
    let mut tokens = Vec::new();
    let mut line_num = 1;
    let mut col_num = 1;
    let mut token_type = TokenType::Undefined;
    let mut tokenizing = false;
    let mut token_start = 0;
    let mut token_col = 1;

    while let Some((pos, ch)) = chars.next() {
        if tokenizing {
            match token_type {
                TokenType::Identifier => {
                    if false == ch.is_alphanumeric() {
                        tokens.push(TokenData::new(
                            token_col,
                            line_num,
                            token_type,
                            &contents[token_start..pos],
                        ));

                        tokenizing = false;
                        token_type = TokenType::Undefined;
                        token_start = pos;
                    }
                }
                _ => println!("Some other token."),
            }
        }

        // Tokenizing can be changed above, so don't merge this to the above if as an else.
        if false == tokenizing {
            if ch.is_alphanumeric() {
                token_type = TokenType::Identifier;
                tokenizing = true;
                token_start = pos;
                token_col = col_num;
            }
        }

        if ch == '\n' {
            line_num += 1;
            col_num = 0;
        }

        col_num += 1;
    }

    println!("Tokens {:#?}", tokens);
}

fn main() {
    run();
}
