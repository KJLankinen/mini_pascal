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
pub struct TokenData {
    pub column: u32, // How much white space from line start to start of this token
    pub line: u32,   // How many \n characters from file start
    pub start: u32, // At which index of the content string does this token start from. A "pointer".
    pub end: u32, // Until which index does this token continue to. Use with start like token_value = &contents[start..end]
    pub token_type: TokenType, // What is the type of this token.
}

impl Default for TokenData {
    fn default() -> TokenData {
        TokenData {
            column: 0,
            line: 0,
            start: 0,
            end: 0,
            token_type: TokenType::Invalid,
        }
    }
}

// =====================================================
// Scanner state
// =====================================================
// Storage for useful variables related to scanning.

#[derive(Debug)]
pub struct ScannerState {
    pub line_number: u32,        // how many \n characters have we found
    pub column_number: u32,      // how many characters since the last \n
    pub scanner_location: u32,   // how many characters from the start of the program string
    pub latest_token: TokenData, // the lates token we have found
    pub contents: String,        // the entire program as a string
}

impl Default for ScannerState {
    fn default() -> ScannerState {
        ScannerState {
            line_number: 0,
            column_number: 0,
            scanner_location: 0,
            latest_token: Default::default(),
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
pub fn get_next_token(scanner_state: &mut ScannerState) -> Result<(), &'static str> {
    scanner_state.latest_token = TokenData {
        column: 0,
        line: 0,
        start: 5,
        end: 16,
        token_type: TokenType::Invalid,
    };
    Ok(())
}
