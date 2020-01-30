use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;
use std::string::String;

fn read_contents(args: Vec<String>) -> Result<String, &'static str> {
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

// All the legal tokens of Mini PL.
// No disctinction between different operator overloads,
// that's for parser to decide.
#[derive(Debug)]
enum TokenType {
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

#[derive(Debug)]
struct TokenData {
    column: u32,
    line: u32,
    start: u32,
    end: u32,
    token_type: TokenType,
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

// Storage for useful variables related to scanning.
#[derive(Debug)]
struct ScannerState {
    line_number: u32,        // how many \n characters have we found
    column_number: u32,      // how many characters since the last \n
    scanner_location: u32,   // how many characters from the start of the program string
    latest_token: TokenData, // the lates token we have found
    contents: String,        // the entire program as a string
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

fn get_next_token(scanner_state: &mut ScannerState) -> Result<(), &'static str> {
    scanner_state.latest_token = TokenData {
        column: 0,
        line: 0,
        start: 5,
        end: 16,
        token_type: TokenType::Invalid,
    };
    Ok(())
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

    let mut scanner_state: ScannerState = Default::default();
    scanner_state.contents = match read_contents(env::args().collect()) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Application error: {}", err);
            process::exit(1);
        }
    };

    if let Err(err) = get_next_token(&mut scanner_state) {
        eprintln!("Error getting next token: {}", err);
        process::exit(1);
    }

    println!("Received token {:#?}", scanner_state.latest_token);

    //for (index, matched) in contents.match_indices("/*")

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
