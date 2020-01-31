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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct TokenData<'a> {
    pub column: u32, // How much white space from line start to start of this token
    pub line: u32,   // How many \n characters from file start
    pub value: &'a str,
    pub token_type: TokenType, // What is the type of this token.
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

    while let Some((start, ch)) = chars.peek() {
        if ch.is_alphanumeric() {
            while let Some((pos, ch)) = chars.peek() {
                if ch.is_alphanumeric() {
                    chars.next();
                    col_num += 1;
                } else {
                    token_type = TokenType::Identifier;
                    tokens.push(TokenData::new(
                        line_num,
                        col_num,
                        token_type,
                        &contents[start..pos],
                    ));

                    break;
                }
            }
        } else if ch == &'\n' {
            line_num += 1;
        }
    }

    println!("Tokens {:#?}", tokens);

    //for word in contents.split_whitespace() {
    //    if let Some(_) = keywords.get(word) {
    //        println!("{}", word);
    //    } else {
    //        println!("{} is not a keyword", word);
    //    }
    //}

    //let mut result = Vec::new();
    //for word in contents.split_whitespace() {
    //    let mut last = 0;
    //    for (index, matched) in word.match_indices(|c: char| !c.is_alphanumeric()) {
    //        if last != index {
    //            result.push(&word[last..index]);
    //        }
    //        result.push(matched);
    //        last = index + matched.len();
    //    }
    //    if last < word.len() {
    //        result.push(&word[last..]);
    //    }
    //}
    //println!("{:?}", result);
}

fn main() {
    run();
}
