use compiler::get_token;
use compiler::Parser;
use compiler::TokenType;

use std::env;
use std::fs;
use std::process;
use std::string::String;

fn read_program_to_string(args: Vec<String>) -> Result<String, &'static str> {
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
