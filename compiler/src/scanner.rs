use std::fs;
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
pub struct TokenData {
    pub column: usize, // How much white space from line start to start of this token
    pub line: usize,   // How many \n characters from file start
    pub start: usize, // At which index of the content string does this token start from. A "pointer".
    pub end: usize, // Until which index does this token continue to. Use with start like token_value = &contents[start..end]
    pub token_type: TokenType, // What is the type of this token.
}

impl TokenData {
    fn new() -> TokenData {
        TokenData {
            column: 0,
            line: 0,
            start: 0,
            end: 0,
            token_type: TokenType::Undefined,
        }
    }
}

// =====================================================
// Scanner state
// =====================================================
// Storage for useful variables related to scanning.

#[derive(Debug)]
pub struct Scanner {
    pub line: usize,             // how many \n characters have we found
    pub column: usize,           // how many characters since the last \n
    pub location: usize,         // how many characters from the start of the program string
    pub latest_token: TokenData, // the lates token we have found
    pub contents: String,        // the entire program as a string
}

impl Scanner {
    pub fn new() -> Scanner {
        Scanner {
            line: 0,
            column: 0,
            location: 0,
            latest_token: TokenData::new(),
            contents: Default::default(),
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

// Parser calls this function to receive tokens
pub fn get_next_token(scanner: &mut Scanner) -> Result<(), &'static str> {
    if scanner.location == scanner.contents.len() {
        return Err("Reached EOF, no more tokens.");
    }

    let mut token_found = false;
    while !token_found && scanner.location < scanner.contents.len() {
        // TODO should use iterators, indexing string by a number is crappy, since rust uses utf8,
        // i.e. multibyte stuff.
        let cur_char = scanner.contents[scanner.location];
        match cur_char {
            ' ' => println!("space: {}", cur_char),
            '\t' => println!("tab: {}", cur_char),
            '\n' => println!("newline: {}", cur_char),
        }
        scanner.location += 1;
    }

    scanner.latest_token = TokenData {
        column: 0,
        line: 0,
        start: 5,
        end: 16,
        token_type: TokenType::Invalid,
    };
    Ok(())
}
